(ns frontend.handler.block
  (:require ["/frontend/utils" :as utils]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [dommy.core :as dom]
            [frontend.db :as db]
            [frontend.db.model :as db-model]
            [frontend.db.react :as react]
            [frontend.format.block :as block]
            [frontend.format.mldoc :as mldoc]
            [frontend.state :as state]
            [frontend.util :as util]
            [goog.dom :as gdom]
            [goog.dom.classes :as gdom-classes]
            [goog.object :as gobj]
            [frontend.handler.dnd :as dnd]))

;;  Fns

(defn long-page?
  [repo page-id]
  (>= (db/get-page-blocks-count repo page-id) db-model/initial-blocks-length))

(defn get-block-refs-with-children
  [block]
  (->>
   (tree-seq :block/refs
             :block/children
             block)
   (mapcat :block/refs)
   (distinct)))

(defn filter-blocks
  [repo ref-blocks filters group-by-page?]
  (let [ref-pages-ids (->> (if group-by-page?
                             (mapcat last ref-blocks)
                             ref-blocks)
                           (mapcat (fn [b] (get-block-refs-with-children b)))
                           (concat (when group-by-page? (map first ref-blocks)))
                           (distinct)
                           (map :db/id)
                           (remove nil?))
        ref-pages (db/pull-many repo '[:db/id :block/name] ref-pages-ids)
        ref-pages (zipmap (map :block/name ref-pages) (map :db/id ref-pages))
        exclude-ids (->> (map (fn [page] (get ref-pages page)) (get filters false))
                         (remove nil?)
                         (set))
        include-ids (->> (map (fn [page] (get ref-pages page)) (get filters true))
                         (remove nil?)
                         (set))]
    (if (empty? filters)
      ref-blocks
      (let [filter-f (fn [ref-blocks]
                       (cond->> ref-blocks
                         (seq exclude-ids)
                         (remove (fn [block]
                                   (let [ids (set (concat (map :db/id (get-block-refs-with-children block))
                                                          [(:db/id (:block/page block))]))]
                                     (seq (set/intersection exclude-ids ids)))))

                         (seq include-ids)
                         (remove (fn [block]
                                   (let [page-block-id (:db/id (:block/page block))
                                         ids (set (map :db/id (get-block-refs-with-children block)))]
                                     (if (and (contains? include-ids page-block-id)
                                              (= 1 (count include-ids)))
                                       (not= page-block-id (first include-ids))
                                       (empty? (set/intersection include-ids (set (conj ids page-block-id))))))))))]
        (if group-by-page?
          (->> (map (fn [[p ref-blocks]]
                      [p (filter-f ref-blocks)]) ref-blocks)
               (remove #(empty? (second %))))
          (->> (filter-f ref-blocks)
               (remove nil?)))))))

;; TODO: reduced version
(defn- walk-block
  [block check? transform]
  (let [result (atom nil)]
    (walk/postwalk
     (fn [x]
       (if (check? x)
         (reset! result (transform x))
         x))
     (:block/body block))
    @result))

(defn get-timestamp
  [block typ]
  (walk-block block
              (fn [x]
                (and (block/timestamp-block? x)
                     (= typ (first (second x)))))
              #(second (second %))))

(defn get-scheduled-ast
  [block]
  (get-timestamp block "Scheduled"))

(defn get-deadline-ast
  [block]
  (get-timestamp block "Deadline"))

(defn load-more!
  [db-id start-id]
  (let [repo (state/get-current-repo)
        db (db/get-db repo)
        block (db/entity repo db-id)
        block? (not (:block/name block))
        k (if block?
            :frontend.db.react/block-and-children
            :frontend.db.react/page-blocks)
        query-k [repo k db-id]
        option (cond-> {:limit db-model/step-loading-blocks}
                 block?
                 (assoc :scoped-block-id db-id))
        more-data (->> (db-model/get-paginated-blocks-no-cache db start-id option)
                       (map #(db/pull (:db/id %))))]
    (react/swap-new-result! query-k
                            (fn [result]
                              (->> (concat result more-data)
                                   (util/distinct-by :db/id))))))

;;;; Block drag event handler
;;;;;; local state
(defonce *dragging?
  (atom false))
(defonce *dragging-block
  (atom nil))
(defonce *drag-to-block
  (atom nil))
(def *move-to (atom nil))

;;;;;; Helper functions
(defn- dnd-same-block?
  [uuid]
  (= (:block/uuid @*dragging-block) uuid))

(defn non-dragging?
  [e]
  (and (= (gobj/get e "buttons") 1)
       (not (dom/has-class? (gobj/get e "target") "bullet-container"))
       (not (dom/has-class? (gobj/get e "target") "bullet"))
       (not @*dragging?)))

(defn highlight-block!
  [block-uuid]
  (let [blocks (array-seq (js/document.getElementsByClassName (str block-uuid)))]
    (doseq [block blocks]
      (dom/add-class! block "block-highlight"))))

(defn unhighlight-blocks!
  []
  (let [blocks (some->> (array-seq (js/document.getElementsByClassName "block-highlight"))
                        (repeat 2)
                        (apply concat))]
    (doseq [block blocks]
      (gdom-classes/remove block "block-highlight"))))

;;;;;; Drag event Listeners
(defn bullet-drag-start
  [event block uuid block-id]
  (highlight-block! uuid)
  (.setData (gobj/get event "dataTransfer")
            "block-uuid"
            uuid)
  (.setData (gobj/get event "dataTransfer")
            "block-dom-id"
            block-id)
  (reset! *dragging? true)
  (reset! *dragging-block block))

(defn block-drag-start
  [event block uuid block-id]
  (bullet-drag-start event block uuid block-id))

(defn block-drag-over
  [event uuid top? block-id *move-to]
  (util/stop event)
  (when-not (dnd-same-block? uuid)
    (let [over-block (gdom/getElement block-id)
          rect (utils/getOffsetRect over-block)
          element-top (gobj/get rect "top")
          element-left (gobj/get rect "left")
          x-offset (- (.. event -pageX) element-left)
          cursor-top (gobj/get event "clientY")
          move-to-value (cond
                          (and top? (<= (js/Math.abs (- cursor-top element-top)) 16))
                          :top

                          (> x-offset 50)
                          :nested

                          :else
                          :sibling)]
      (reset! *drag-to-block block-id)
      (reset! *move-to move-to-value))))

(defn block-drag-leave
  [*move-to]
  (reset! *move-to nil))

(defn block-drop
  [event uuid target-block *move-to]
  (util/stop event)
  (when-not (dnd-same-block? uuid)
    (let [block-uuids (state/get-selection-block-ids)
          lookup-refs (map (fn [id] [:block/uuid id]) block-uuids)
          selected (db/pull-many (state/get-current-repo) '[*] lookup-refs)
          blocks (if (seq selected) selected [@*dragging-block])]
      (dnd/move-blocks event blocks target-block @*move-to)))
  (reset! *dragging? false)
  (reset! *dragging-block nil)
  (reset! *drag-to-block nil)
  (reset! *move-to nil)
  (unhighlight-blocks!))

(defn block-drag-end
  [_event *move-to]
  (reset! *dragging? false)
  (reset! *dragging-block nil)
  (reset! *drag-to-block nil)
  (reset! *move-to nil)
  (unhighlight-blocks!))

(defn on-drag-and-mouse-attrs
  [block uuid top? block-id *move-to]
  {:on-drag-over (fn [event]
                   (block-drag-over event uuid top? block-id *move-to))
   :on-drag-leave (fn [_event]
                    (block-drag-leave *move-to))
   :on-drop (fn [event]
              (block-drop event uuid block *move-to))
   :on-drag-end (fn [event]
                  (block-drag-end event *move-to))})

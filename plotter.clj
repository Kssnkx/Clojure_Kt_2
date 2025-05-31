(ns plotter.core
  (:gen-class))

;; Словарь цветов
(def colors
  {:black "чёрный"
   :red "красный"
   :green "зелёный"})

;; Состояние плоттера: неизменяемая мапа
;; {:point {:x <число> :y <число>}
;;  :angle <число> ; угол в градусах
;;  :pen-color :black/:red/:green
;;  :pen :up/:down}

;; Нормализация угла до [0, 360)
(defn normalize-angle [angle]
  (mod angle 360))

;; Рисование линии
(defn draw-segment
  "Рисует линию от точки start до end с указанным цветом."
  [printer start end color]
  (printer (format "Чертится линия: (%d, %d) → (%d, %d), цвет %s."
                   (:x start) (:y start)
                   (:x end) (:y end)
                   (colors color))))

;; Вычисление новой позиции
(defn new-point
  [distance angle point]
  (let [radians (* angle (/ Math/PI 180.0))
        x-new (Math/round (+ (:x point) (* distance (Math/cos radians))))
        y-new (Math/round (+ (:y point) (* distance (Math/sin radians))))]
    {:x x-new :y y-new}))

;; Перемещение каретки
(defn shift
  [printer distance state]
  (let [current (:point state)
        next-point (new-point distance (:angle state) current)]
    (if (= (:pen state) :down)
      (draw-segment printer current next-point (:pen-color state))
      (printer (format "Смещение на %d от (%d, %d)." distance (:x current) (:y current))))
    (assoc state :point next-point)))

;; Поворот каретки
(defn rotate
  [printer degrees state]
  (printer (format "Поворот на %d градусов." degrees))
  (update state :angle #(normalize-angle (+ % degrees))))

;; Поднять каретку
(defn pen-up
  [printer state]
  (printer "Каретка поднята.")
  (assoc state :pen :up))

;; Опустить каретку
(defn pen-down
  [printer state]
  (printer "Каретка опущена.")
  (assoc state :pen :down))

;; Установить цвет
(defn set-pen-color
  [printer color state]
  (printer (format "Цвет линии: %s." (colors color)))
  (assoc state :pen-color color))

;; Установить позицию
(defn set-point
  [printer point state]
  (printer (format "Каретка на позиции (%d, %d)." (:x point) (:y point)))
  (assoc state :point point))

;; Рисование треугольника
(defn draw-triangle
  [printer size state]
  (let [state-down (pen-down printer state)]
    (reduce
      (fn [s _]
        (-> s
            (shift printer size)
            (rotate printer 120)))
      state-down
      (range 3))))

;; Рисование квадрата
(defn draw-square
  [printer size state]
  (let [state-down (pen-down printer state)]
    (reduce
      (fn [s _]
        (-> s
            (shift printer size)
            (rotate printer 90)))
      state-down
      (range 4))))

;; Инициализация состояния
(defn init-plotter
  [point angle color pen]
  {:point point
   :angle angle
   :pen-color color
   :pen pen})

;; Точка входа
(defn -main
  [& args]
  (let [printer println
        initial (init-plotter {:x 0 :y 0} 0 :black :up)
        after-triangle (draw-triangle printer 100 initial)
        after-move (set-point printer {:x 10 :y 10} after-triangle)
        after-color (set-pen-color printer :red after-move)
        final (draw-square printer 80 after-color)]
    (println "Итоговое состояние:" final)))

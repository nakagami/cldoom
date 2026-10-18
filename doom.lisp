;;;; ====================================================================
;;;; CL-DOOM: 3D Raycasting First-Person Shooter for ExLisp / BEAM
;;;; ====================================================================

;; --- ANSI Terminal Utilities ---
(defparameter *esc* (code-char 27))
(defparameter *csi* (format nil "~A[" *esc*))
(defparameter *crlf* (format nil "~C~C" (code-char 13) (code-char 10)))

(defun put-line (str)
  (IO.write (format nil "~A~A" str *crlf*)))

(defun ansi-color (code str)
  (format nil "~A~Am~A~A0m" *csi* code str *csi*))

(defun clear-screen ()
  (IO.write (format nil "~A2J~AH" *csi* *csi*)))

(defun cursor-home ()
  (IO.write (format nil "~AH" *csi*)))

(defun hide-cursor ()
  (IO.write (format nil "~A?25l" *csi*)))

(defun show-cursor ()
  (IO.write (format nil "~A?25h" *csi*)))

;; --- Screen & Raycasting Constants ---
(defparameter *screen-w* 72)
(defparameter *screen-h* 20)
(defparameter *fov* 1.0471975) ;; 60 degrees in radians
(defparameter *pi* 3.141592653589793)
(defparameter *two-pi* 6.283185307179586)

;; --- Helper Math Functions ---
(defun normalize-angle (ang)
  (let ((a (rem ang *two-pi*)))
    (cond
      ((< a 0.0) (+ a *two-pi*))
      ((>= a *two-pi*) (- a *two-pi*))
      (t a))))

(defun dist2 (x1 y1 x2 y2)
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (:math:sqrt (+ (* dx dx) (* dy dy)))))

;; --- Maps & Levels ---
;; Map characters:
;; # = Stone Wall, B = Brick Wall, T = Tech Wall, D = Door, E = Exit Gate
;; . = Floor, ~ = Acid/Slime
(defparameter *maps*
  (list
   ;; Level 1: E1M1 - Hangar
   (list
    :name "E1M1: HANGAR"
    :width 24
    :height 16
    :data
    ["########################"
     "#......#........#......#"
     "#.##...#..####..#...##.#"
     "#.#....D..#..#..D....#.#"
     "#.#....#..####..#....#.#"
     "#......#........#......#"
     "####D#####....#####D####"
     "#......................#"
     "#..TTTT..........TTTT..#"
     "#..T..T....~~....T..T..#"
     "#..TTTT....~~....TTTT..#"
     "#......................#"
     "####D#####....#####D####"
     "#......#........#....E.#"
     "#..##..D..####..D..###.#"
     "########################"]
    :start-x 3.5
    :start-y 3.5
    :start-angle 0.0
    ;; Initial sprites: (type x y hp max-hp state alert id)
    :sprites
    (list
     (list :type :zombie :x 6.5 :y 3.5 :hp 25 :max-hp 25 :state :idle :alert 0 :id 1)
     (list :type :zombie :x 18.5 :y 3.5 :hp 25 :max-hp 25 :state :idle :alert 0 :id 2)
     (list :type :imp    :x 12.0 :y 8.0 :hp 40 :max-hp 40 :state :idle :alert 0 :id 3)
     (list :type :imp    :x 12.0 :y 10.0 :hp 40 :max-hp 40 :state :idle :alert 0 :id 4)
     (list :type :zombie :x 3.5 :y 14.5 :hp 25 :max-hp 25 :state :idle :alert 0 :id 5)
     (list :type :caco   :x 19.5 :y 14.0 :hp 100 :max-hp 100 :state :idle :alert 0 :id 6)
     ;; Items & Barrels
     (list :type :barrel :x 10.0 :y 8.0 :hp 10 :max-hp 10 :state :idle :alert 0 :id 10)
     (list :type :barrel :x 14.0 :y 8.0 :hp 10 :max-hp 10 :state :idle :alert 0 :id 11)
     (list :type :medkit :x 2.5 :y 5.0 :hp 1 :max-hp 1 :state :idle :alert 0 :id 20)
     (list :type :armor  :x 21.5 :y 3.5 :hp 1 :max-hp 1 :state :idle :alert 0 :id 21)
     (list :type :ammo   :x 12.0 :y 2.5 :hp 1 :max-hp 1 :state :idle :alert 0 :id 22)
     (list :type :shotgun-item :x 3.5 :y 13.5 :hp 1 :max-hp 1 :state :idle :alert 0 :id 23)
     (list :type :medkit :x 21.5 :y 13.5 :hp 1 :max-hp 1 :state :idle :alert 0 :id 24)))

   ;; Level 2: E1M2 - Nuclear Plant
   (list
    :name "E1M2: NUCLEAR PLANT"
    :width 24
    :height 16
    :data
    ["########################"
     "#..TT..#........#..TT..#"
     "#.TTTT.#.BB..BB.#.TTTT.#"
     "#..TT..D.B....B.D..TT..#"
     "#......#.BB..BB.#......#"
     "###D######....######D###"
     "#......#........#......#"
     "#..~~..#..TTTT..#..~~..#"
     "#..~~..D..T..T..D..~~..#"
     "#..~~..#..TTTT..#..~~..#"
     "#......#........#......#"
     "###D######....######D###"
     "#......#.BB..BB.#....E.#"
     "#.TTTT.#.B....B.D..###.#"
     "#..TT..#.........#.....#"
     "########################"]
    :start-x 2.5
    :start-y 2.5
    :start-angle 1.57
    :sprites
    (list
     (list :type :imp    :x 5.5 :y 4.5 :hp 40 :max-hp 40 :state :idle :alert 0 :id 1)
     (list :type :imp    :x 18.5 :y 4.5 :hp 40 :max-hp 40 :state :idle :alert 0 :id 2)
     (list :type :zombie :x 12.0 :y 4.0 :hp 25 :max-hp 25 :state :idle :alert 0 :id 3)
     (list :type :caco   :x 12.0 :y 8.5 :hp 100 :max-hp 100 :state :idle :alert 0 :id 4)
     (list :type :imp    :x 4.0 :y 13.0 :hp 40 :max-hp 40 :state :idle :alert 0 :id 5)
     (list :type :caco   :x 19.0 :y 13.0 :hp 100 :max-hp 100 :state :idle :alert 0 :id 6)
     (list :type :barrel :x 11.0 :y 7.5 :hp 10 :max-hp 10 :state :idle :alert 0 :id 10)
     (list :type :barrel :x 13.0 :y 7.5 :hp 10 :max-hp 10 :state :idle :alert 0 :id 11)
     (list :type :bfg-item :x 12.0 :y 9.0 :hp 1 :max-hp 1 :state :idle :alert 0 :id 20)
     (list :type :ammo   :x 2.5 :y 14.0 :hp 1 :max-hp 1 :state :idle :alert 0 :id 21)
     (list :type :armor  :x 21.5 :y 2.5 :hp 1 :max-hp 1 :state :idle :alert 0 :id 22)
     (list :type :medkit :x 21.5 :y 14.0 :hp 1 :max-hp 1 :state :idle :alert 0 :id 23)))

   ;; Level 3: E1M8 - Phobos Anomaly (Boss Arena)
   (list
    :name "E1M8: PHOBOS ANOMALY"
    :width 20
    :height 16
    :data
    ["####################"
     "#..................#"
     "#..TT..........TT..#"
     "#..TT..........TT..#"
     "#........~~........#"
     "#.......~~~~.......#"
     "#..BB....~~....BB..#"
     "#..BB..........BB..#"
     "#..BB..........BB..#"
     "#..BB....~~....BB..#"
     "#.......~~~~.......#"
     "#........~~........#"
     "#..TT..........TT..#"
     "#..TT..........TT.E#"
     "#..................#"
     "####################"]
    :start-x 10.0
    :start-y 2.5
    :start-angle 1.57
    :sprites
    (list
     (list :type :caco   :x 7.0 :y 8.0 :hp 150 :max-hp 150 :state :idle :alert 0 :id 1)
     (list :type :caco   :x 13.0 :y 8.0 :hp 150 :max-hp 150 :state :idle :alert 0 :id 2)
     (list :type :imp    :x 5.0 :y 12.0 :hp 40 :max-hp 40 :state :idle :alert 0 :id 3)
     (list :type :imp    :x 15.0 :y 12.0 :hp 40 :max-hp 40 :state :idle :alert 0 :id 4)
     (list :type :barrel :x 9.0 :y 8.0 :hp 10 :max-hp 10 :state :idle :alert 0 :id 10)
     (list :type :barrel :x 11.0 :y 8.0 :hp 10 :max-hp 10 :state :idle :alert 0 :id 11)
     (list :type :medkit :x 3.0 :y 3.0 :hp 1 :max-hp 1 :state :idle :alert 0 :id 20)
     (list :type :medkit :x 17.0 :y 3.0 :hp 1 :max-hp 1 :state :idle :alert 0 :id 21)
     (list :type :ammo   :x 10.0 :y 4.0 :hp 1 :max-hp 1 :state :idle :alert 0 :id 22)
     (list :type :armor  :x 10.0 :y 13.0 :hp 1 :max-hp 1 :state :idle :alert 0 :id 23)))
  ))

;; --- Global Game State ---
(defparameter *current-level-index* 0)
(defparameter *map-data* nil)
(defparameter *map-w* 24)
(defparameter *map-h* 16)
(defparameter *level-name* "")
(defparameter *discovered-map* nil)

(defparameter *player-x* 3.5)
(defparameter *player-y* 3.5)
(defparameter *player-angle* 0.0)
(defparameter *player-hp* 100)
(defparameter *player-max-hp* 100)
(defparameter *player-armor* 50)
(defparameter *player-ammo-bullets* 50)
(defparameter *player-ammo-shells* 20)
(defparameter *player-ammo-cells* 40)
(defparameter *player-weapon* 3) ;; 1=Fist, 2=Pistol, 3=Shotgun, 4=BFG
(defparameter *has-shotgun* t)
(defparameter *has-bfg* nil)
(defparameter *player-kills* 0)
(defparameter *player-face* "^_^")
(defparameter *player-face-timer* 0)

(defparameter *sprites* nil)
(defparameter *messages* '("Welcome to DOOM! Rip and Tear!"))
(defparameter *flash-type* nil) ;; :hit, :shoot, :pickup
(defparameter *flash-timer* 0)
(defparameter *show-minimap* nil)
(defparameter *game-over* nil)
(defparameter *level-cleared* nil)
(defparameter *game-won* nil)

;; --- Map Query & Manipulation ---
(defun get-map-cell (x y)
  (if (and (>= x 0) (< x *map-w*) (>= y 0) (< y *map-h*))
      (elt (elt *map-data* y) x)
      #\#))

(defun set-map-cell (x y ch)
  (when (and (>= x 0) (< x *map-w*) (>= y 0) (< y *map-h*))
    (let ((row (elt *map-data* y)))
      (let ((row-chars (coerce row 'list)))
        (let ((new-row-chars (let ((idx 0))
                               (mapcar (lambda (c)
                                         (prog1 (if (= idx x) ch c)
                                           (setq idx (1+ idx))))
                                       row-chars))))
          (let ((new-row (coerce new-row-chars 'string)))
            (setf (elt *map-data* y) new-row)))))))

(defun is-wall-solid (ch)
  (or (char= ch #\#) (char= ch #\B) (char= ch #\T) (char= ch #\D)))

(defun log-msg (msg)
  (setq *messages* (cons msg *messages*))
  (when (> (length *messages*) 3)
    (setq *messages* (subseq *messages* 0 3))))

;; --- Level Loading ---
(defun load-level (idx)
  (let ((lvl (nth idx *maps*)))
    (setq *current-level-index* idx)
    (setq *level-name* (getf lvl :name))
    (setq *map-w* (getf lvl :width))
    (setq *map-h* (getf lvl :height))
    (setq *map-data* (copy-list (getf lvl :data)))
    (setq *player-x* (getf lvl :start-x))
    (setq *player-y* (getf lvl :start-y))
    (setq *player-angle* (getf lvl :start-angle))
    (setq *sprites* (mapcar #'copy-list (getf lvl :sprites)))
    (setq *discovered-map* (make-array (list *map-h* *map-w*) :initial-element nil))
    (setq *level-cleared* nil)
    (log-msg (format nil "Entering ~A" *level-name*))))

;; --- Raycasting Renderer ---
(defun render-view ()
  (let* ((half-fov (/ *fov* 2.0))
         (z-buffer (make-array *screen-w* :initial-element 999.0))
         (screen (make-array (list *screen-h* *screen-w*) :initial-element #\Space))
         (color-grid (make-array (list *screen-h* *screen-w*) :initial-element "0")))

    ;; 1. Reveal map around player
    (let ((px-int (floor *player-x*))
          (py-int (floor *player-y*)))
      (loop for dy from -3 to 3 do
        (loop for dx from -3 to 3 do
          (let ((nx (+ px-int dx))
                (ny (+ py-int dy)))
            (when (and (>= nx 0) (< nx *map-w*) (>= ny 0) (< ny *map-h*))
              (setf (aref *discovered-map* ny nx) t))))))

    ;; 2. Cast DDA Rays for Walls
    (dotimes (col *screen-w*)
      (let* ((ray-angle (normalize-angle (+ (- *player-angle* half-fov)
                                            (* (/ (* col 1.0) *screen-w*) *fov*))))
             (rdx (:math:cos ray-angle))
             (rdy (:math:sin ray-angle))
             (map-x (floor *player-x*))
             (map-y (floor *player-y*))
             (delta-dist-x (if (= rdx 0.0) 1e30 (abs (/ 1.0 rdx))))
             (delta-dist-y (if (= rdy 0.0) 1e30 (abs (/ 1.0 rdy))))
             (step-x (if (< rdx 0) -1 1))
             (step-y (if (< rdy 0) -1 1))
             (side-dist-x (if (< rdx 0)
                              (* (- *player-x* map-x) delta-dist-x)
                              (* (- (+ map-x 1.0) *player-x*) delta-dist-x)))
             (side-dist-y (if (< rdy 0)
                              (* (- *player-y* map-y) delta-dist-y)
                              (* (- (+ map-y 1.0) *player-y*) delta-dist-y)))
             (hit nil)
             (side 0)
             (wall-ch #\#)
             (steps 0))
        (loop while (and (not hit) (< steps 40)) do
          (setq steps (1+ steps))
          (if (< side-dist-x side-dist-y)
              (progn
                (setq side-dist-x (+ side-dist-x delta-dist-x))
                (setq map-x (+ map-x step-x))
                (setq side 0))
              (progn
                (setq side-dist-y (+ side-dist-y delta-dist-y))
                (setq map-y (+ map-y step-y))
                (setq side 1)))
          (let ((ch (get-map-cell map-x map-y)))
            ;; Reveal discovered walls
            (when (and (>= map-x 0) (< map-x *map-w*) (>= map-y 0) (< map-y *map-h*))
              (setf (aref *discovered-map* map-y map-x) t))
            (when (is-wall-solid ch)
              (setq hit t)
              (setq wall-ch ch))))

        (let* ((raw-dist (if (= side 0)
                             (- side-dist-x delta-dist-x)
                             (- side-dist-y delta-dist-y)))
               ;; Fish-eye correction
               (dist (* raw-dist (:math:cos (- ray-angle *player-angle*))))
               (dist (max dist 0.1)))
          ;; Store in Z-buffer for sprites
          (setf (aref z-buffer col) dist)

          (let* ((wall-h (min *screen-h* (floor (/ *screen-h* dist))))
                 (draw-start (max 0 (floor (/ (- *screen-h* wall-h) 2.0))))
                 (draw-end (min (1- *screen-h*) (+ draw-start wall-h)))
                 ;; Wall color code
                 (base-color (cond
                               ((char= wall-ch #\#) (if (= side 0) "37;1" "37"))   ;; Stone: White/Grey
                               ((char= wall-ch #\B) (if (= side 0) "31;1" "31"))   ;; Brick: Red
                               ((char= wall-ch #\T) (if (= side 0) "36;1" "34"))   ;; Tech: Cyan/Blue
                               ((char= wall-ch #\D) (if (= side 0) "33;1" "33"))   ;; Door: Yellow
                               ((char= wall-ch #\E) "32;1")                        ;; Exit: Green
                               (t "37")))
                 ;; Wall character shade
                 (shade-ch (cond
                             ((char= wall-ch #\D) (if (= side 0) #\| #\=))
                             ((char= wall-ch #\E) #\X)
                             ((< dist 2.5) #\#)
                             ((< dist 4.5) #\O)
                             ((< dist 7.0) #\*)
                             ((< dist 10.0) #\-)
                             (t #\.))))

            ;; Render column (Ceiling, Wall, Floor)
            (dotimes (row *screen-h*)
              (cond
                ((< row draw-start)
                 ;; Ceiling / Sky
                 (setf (aref screen row col) #\Space)
                 (setf (aref color-grid row col) "0"))
                ((<= row draw-end)
                 ;; Wall
                 (setf (aref screen row col) shade-ch)
                 (setf (aref color-grid row col) base-color))
                (t
                 ;; Floor / Acid
                 (let ((floor-dist (/ *screen-h* (+ (* 2.0 (- row (/ *screen-h* 2.0))) 0.001))))
                   (if (and (> row (+ draw-end 2)) (< floor-dist 3.0))
                       (progn
                         (setf (aref screen row col) #\.)
                         (setf (aref color-grid row col) "30;1"))
                       (progn
                         (setf (aref screen row col) #\Space)
                         (setf (aref color-grid row col) "0")))))))))))

    ;; 3. Render Sprites (sorted by distance)
    (let* ((active-sprites (remove-if (lambda (s) (eq (getf s :state) :dead)) *sprites*))
           (sprites-with-dist (mapcar (lambda (s)
                                        (let ((d (dist2 *player-x* *player-y* (getf s :x) (getf s :y))))
                                          (list :sprite s :dist d)))
                                      active-sprites))
           (sorted-sprites (Enum.sort_by sprites-with-dist (lambda (sd) (- (getf sd :dist))))))

      (dolist (sd sorted-sprites)
        (let* ((s (getf sd :sprite))
               (stype (getf s :type))
               (sx (getf s :x))
               (sy (getf s :y))
               (dir-x (:math:cos *player-angle*))
               (dir-y (:math:sin *player-angle*))
               (plane-len (:math:tan half-fov))
               (plane-x (* (- dir-y) plane-len))
               (plane-y (* dir-x plane-len))
               (dx (- sx *player-x*))
               (dy (- sy *player-y*))
               (inv-det (/ 1.0 (- (* plane-x dir-y) (* dir-x plane-y))))
               (trans-x (* inv-det (- (* dir-y dx) (* dir-x dy))))
               (trans-y (* inv-det (+ (* (- plane-y) dx) (* plane-x dy)))))

          ;; Only render if in front of player
          (when (> trans-y 0.3)
            (let* ((screen-x (floor (* (/ *screen-w* 2.0) (+ 1.0 (/ trans-x trans-y)))))
                   (spr-h (max 1 (floor (/ *screen-h* trans-y))))
                   (spr-w (max 1 (floor (/ *screen-h* trans-y))))
                   (draw-y-start (max 0 (floor (+ (/ (- *screen-h* spr-h) 2.0) (/ spr-h 4.0)))))
                   (draw-y-end (min (1- *screen-h*) (+ draw-y-start spr-h)))
                   (draw-x-start (max 0 (floor (- screen-x (/ spr-w 2.0)))))
                   (draw-x-end (min (1- *screen-w*) (floor (+ screen-x (/ spr-w 2.0)))))
                   (char-color (cond
                                 ((eq stype :imp) (list #\D "31;1"))           ;; Demon: Bright Red D
                                 ((eq stype :zombie) (list #\Z "32;1"))        ;; Zombie: Green Z
                                 ((eq stype :caco) (list #\C "35;1"))          ;; Cacodemon: Magenta C
                                 ((eq stype :barrel) (list #\B "33;1"))        ;; Barrel: Yellow B
                                 ((eq stype :medkit) (list #\+ "92;1"))        ;; Medkit: Bright Green +
                                 ((eq stype :armor) (list #\A "96;1"))         ;; Armor: Cyan A
                                 ((eq stype :ammo) (list #\S "93;1"))          ;; Ammo: Yellow S
                                 ((eq stype :shotgun-item) (list #\W "97;1"))  ;; Shotgun: White W
                                 ((eq stype :bfg-item) (list #\G "95;1"))      ;; BFG: Purple G
                                 (t (list #\? "37"))))
                   (spr-char (first char-color))
                   (spr-color (second char-color)))

              (loop for stripe from draw-x-start to draw-x-end do
                (when (and (>= stripe 0) (< stripe *screen-w*) (< trans-y (aref z-buffer stripe)))
                  (loop for r from draw-y-start to draw-y-end do
                    (when (and (>= r 0) (< r *screen-h*))
                      (setf (aref screen r stripe) spr-char)
                      (setf (aref color-grid r stripe) spr-color))))))))))

    ;; 4. Draw Crosshair in Center
    (let ((mid-r (floor (/ *screen-h* 2)))
          (mid-c (floor (/ *screen-w* 2))))
      (when (char= (aref screen mid-r mid-c) #\Space)
        (setf (aref screen mid-r mid-c) #\+)
        (setf (aref color-grid mid-r mid-c) "30;1")))

    ;; 5. Draw Weapon at Bottom Center
    (let ((mid-c (floor (/ *screen-w* 2)))
          (bot-r (1- *screen-h*)))
      (cond
        ((and *flash-type* (eq *flash-type* :shoot))
         ;; Muzzle Flash Frame!
         (let ((flash-art ["  \\  |  /  "
                           "-- *BOOM* --"
                           "  /  |  \\  "]))
           (dotimes (fi 3)
             (let ((row (- bot-r (- 2 fi)))
                   (str (elt flash-art fi)))
               (dotimes (fj (length str))
                 (let ((col (+ (- mid-c 6) fj))
                       (ch (elt str fj)))
                   (unless (char= ch #\Space)
                     (when (and (>= col 0) (< col *screen-w*) (>= row 0) (< row *screen-h*))
                       (setf (aref screen row col) ch)
                       (setf (aref color-grid row col) "93;1;41")))))))))

        ;; Weapon Idle Display
        ((= *player-weapon* 3)
         ;; Shotgun
         (let ((gun-art ["  /===\\  "
                         " | (o) | "
                         " | === | "]))
           (dotimes (gi 3)
             (let ((row (- bot-r (- 2 gi)))
                   (str (elt gun-art gi)))
               (dotimes (gj (length str))
                 (let ((col (+ (- mid-c 4) gj))
                       (ch (elt str gj)))
                   (unless (char= ch #\Space)
                     (when (and (>= col 0) (< col *screen-w*) (>= row 0) (< row *screen-h*))
                       (setf (aref screen row col) ch)
                       (setf (aref color-grid row col) "37;1")))))))))

        ((= *player-weapon* 4)
         ;; BFG 9000
         (let ((gun-art [" <[#BFG#]> "
                         "  / | | \\  "
                         " /__| |__\\ "]))
           (dotimes (gi 3)
             (let ((row (- bot-r (- 2 gi)))
                   (str (elt gun-art gi)))
               (dotimes (gj (length str))
                 (let ((col (+ (- mid-c 5) gj))
                       (ch (elt str gj)))
                   (unless (char= ch #\Space)
                     (when (and (>= col 0) (< col *screen-w*) (>= row 0) (< row *screen-h*))
                       (setf (aref screen row col) ch)
                       (setf (aref color-grid row col) "92;1")))))))))

        ((= *player-weapon* 2)
         ;; Pistol
         (let ((gun-art ["   [||]   "
                         "  [====]  "
                         "   |  |   "]))
           (dotimes (gi 3)
             (let ((row (- bot-r (- 2 gi)))
                   (str (elt gun-art gi)))
               (dotimes (gj (length str))
                 (let ((col (+ (- mid-c 5) gj))
                       (ch (elt str gj)))
                   (unless (char= ch #\Space)
                     (when (and (>= col 0) (< col *screen-w*) (>= row 0) (< row *screen-h*))
                       (setf (aref screen row col) ch)
                       (setf (aref color-grid row col) "33;1")))))))))

        (t
         ;; Fist
         (let ((gun-art ["  .---.  "
                         " / /\"\\ \\ "
                         " | |O| | "]))
           (dotimes (gi 3)
             (let ((row (- bot-r (- 2 gi)))
                   (str (elt gun-art gi)))
               (dotimes (gj (length str))
                 (let ((col (+ (- mid-c 4) gj))
                       (ch (elt str gj)))
                   (unless (char= ch #\Space)
                     (when (and (>= col 0) (< col *screen-w*) (>= row 0) (< row *screen-h*))
                       (setf (aref screen row col) ch)
                       (setf (aref color-grid row col) "33")))))))))))

    ;; 6. Print 3D Screen Rows with ANSI formatting
    (cursor-home)
    ;; Top border / Header
    (let ((hdr (format nil "=== ~A | EXLISP 3D DOOM ENGINE ===" *level-name*)))
      (put-line (ansi-color "31;1" hdr)))

    (dotimes (r *screen-h*)
      (let ((line-acc ""))
        (dotimes (c *screen-w*)
          (let ((ch (aref screen r c))
                (color (aref color-grid r c)))
            (if (string= color "0")
                (setq line-acc (format nil "~A~C" line-acc ch))
                (setq line-acc (format nil "~A~A" line-acc (ansi-color color (string ch)))))))
        (put-line line-acc)))

    ;; 7. DOOM Classic Status Bar / HUD
    (render-hud)

    ;; 8. Event Messages
    (render-messages)

    ;; 9. Optional Minimap
    (when *show-minimap*
      (render-minimap-overlay))))

;; --- HUD & Status Bar ---
(defun render-hud ()
  (let* ((wname (cond
                  ((= *player-weapon* 1) "FIST")
                  ((= *player-weapon* 2) "PISTOL")
                  ((= *player-weapon* 3) "SHOTGUN")
                  ((= *player-weapon* 4) "BFG 9000")
                  (t "NONE")))
         (ammo-str (cond
                     ((= *player-weapon* 1) "INF")
                     ((= *player-weapon* 2) (format nil "~A" *player-ammo-bullets*))
                     ((= *player-weapon* 3) (format nil "~A" *player-ammo-shells*))
                     ((= *player-weapon* 4) (format nil "~A" *player-ammo-cells*))
                     (t "0")))
         (hp-col (if (< *player-hp* 25) "31;1;5" (if (< *player-hp* 50) "33;1" "32;1")))
         (arm-col "36;1")
         (amm-col "33;1")
         (face-col (if (string= *player-face* "x_x") "31;1" "97;1"))
         (hud-border "+----------------------------------------------------------------------+")
         (hud-content
          (format nil "| HP:~A | ARM:~A | [~A] | AMMO:~A | WPN:~A | KILLS:~A |"
                  (ansi-color hp-col (format nil "~3D%" *player-hp*))
                  (ansi-color arm-col (format nil "~3D%" *player-armor*))
                  (ansi-color face-col (format nil "~A" *player-face*))
                  (ansi-color amm-col (format nil "~3A" ammo-str))
                  (ansi-color "35;1" (format nil "~-7A" wname))
                  (ansi-color "91;1" (format nil "~2D" *player-kills*)))))
    (put-line (ansi-color "34;1" hud-border))
    (put-line hud-content)
    (put-line (ansi-color "34;1" hud-border))))

(defun render-messages ()
  (let ((msg (if *messages* (car *messages*) "")))
    (put-line (ansi-color "93;1" (format nil ">> ~A" msg)))
    (put-line (ansi-color "90" "Controls: [W/S] Move | [A/D] Strafe | [Q/E] Turn | [SPACE/F] Fire | [1-4] Wpn | [M] Map | [X] Quit"))))

;; --- Minimap Overlay ---
(defun render-minimap-overlay ()
  (put-line (ansi-color "36;1" "--- AUTOMAP ---"))
  (dotimes (y *map-h*)
    (let ((line-acc ""))
      (dotimes (x *map-w*)
        (let ((px-int (floor *player-x*))
              (py-int (floor *player-y*))
              (is-disc (aref *discovered-map* y x)))
          (cond
            ((and (= x px-int) (= y py-int))
             ;; Player
             (setq line-acc (format nil "~A~A" line-acc (ansi-color "92;1" "@"))))
            ((not is-disc)
             (setq line-acc (format nil "~A " line-acc)))
            (t
             (let ((ch (get-map-cell x y))
                   (has-sprite (find-if (lambda (s)
                                          (and (not (eq (getf s :state) :dead))
                                               (= (floor (getf s :x)) x)
                                               (= (floor (getf s :y)) y)))
                                        *sprites*)))
               (if has-sprite
                   (setq line-acc (format nil "~A~A" line-acc (ansi-color "91;1" "!")))
                   (setq line-acc (format nil "~A~C" line-acc ch))))))))
      (put-line line-acc))))

;; --- Player Movement & Collision ---
(defun try-move-player (dx dy)
  (let* ((new-x (+ *player-x* dx))
         (new-y (+ *player-y* dy))
         (cell (get-map-cell (floor new-x) (floor new-y))))
    (cond
      ((char= cell #\E)
       ;; Exit Gate reached!
       (setq *level-cleared* t)
       (log-msg "EXIT REACHED! Level Complete!"))
      ((char= cell #\D)
       (log-msg "Door opened!"))
      ((is-wall-solid cell)
       (log-msg "Ouch! You bumped into a wall."))
      (t
       (setq *player-x* new-x)
       (setq *player-y* new-y)
       ;; Check hazard
       (when (char= cell #\~)
         (setq *player-hp* (max 0 (- *player-hp* 5)))
         (log-msg "Toxic Sludge burns! -5 HP")
         (setq *flash-type* :hit)
         (setq *player-face* ">_<"))
       ;; Check item pickups
       (check-item-pickups)))))

(defun check-item-pickups ()
  (dolist (s *sprites*)
    (when (not (eq (getf s :state) :dead))
      (let* ((stype (getf s :type))
             (d (dist2 *player-x* *player-y* (getf s :x) (getf s :y))))
        (when (< d 0.7)
          (cond
            ((eq stype :medkit)
             (setq *player-hp* (min *player-max-hp* (+ *player-hp* 25)))
             (setf (getf s :state) :dead)
             (setq *flash-type* :pickup)
             (log-msg "Picked up a Medkit! (+25% Health)"))
            ((eq stype :armor)
             (setq *player-armor* (min 100 (+ *player-armor* 50)))
             (setf (getf s :state) :dead)
             (setq *flash-type* :pickup)
             (log-msg "Picked up Green Armor! (+50% Armor)"))
            ((eq stype :ammo)
             (setq *player-ammo-shells* (+ *player-ammo-shells* 10))
             (setq *player-ammo-bullets* (+ *player-ammo-bullets* 30))
             (setf (getf s :state) :dead)
             (setq *flash-type* :pickup)
             (log-msg "Picked up Ammo Box! (+10 Shells, +30 Bullets)"))
            ((eq stype :shotgun-item)
             (setq *has-shotgun* t)
             (setq *player-weapon* 3)
             (setq *player-ammo-shells* (+ *player-ammo-shells* 8))
             (setf (getf s :state) :dead)
             (setq *flash-type* :pickup)
             (log-msg "Found a SHOTGUN! Rip and Tear!"))
            ((eq stype :bfg-item)
             (setq *has-bfg* t)
             (setq *player-weapon* 4)
             (setq *player-ammo-cells* (+ *player-ammo-cells* 40))
             (setf (getf s :state) :dead)
             (setq *flash-type* :pickup)
             (log-msg "FOUND THE BFG 9000! MAXIMUM DESTRUCTION!"))))))))

;; --- Weapon Firing & Combat ---
(defun fire-weapon ()
  (setq *flash-type* :shoot)
  (setq *flash-timer* 1)
  (setq *player-face* "Ò_Ó")
  (setq *player-face-timer* 2)

  ;; Check Ammo
  (let ((can-fire t)
        (dmg 0)
        (wpn-name ""))
    (cond
      ((= *player-weapon* 1)
       ;; Fist
       (setq dmg (+ 15 (random 15)))
       (setq wpn-name "Fist"))
      ((= *player-weapon* 2)
       ;; Pistol
       (if (>= *player-ammo-bullets* 1)
           (progn
             (setq *player-ammo-bullets* (1- *player-ammo-bullets*))
             (setq dmg (+ 20 (random 20)))
             (setq wpn-name "Pistol"))
           (progn (setq can-fire nil) (log-msg "Pistol out of bullets! *CLICK*"))))
      ((= *player-weapon* 3)
       ;; Shotgun
       (if (>= *player-ammo-shells* 1)
           (progn
             (setq *player-ammo-shells* (1- *player-ammo-shells*))
             (setq dmg (+ 60 (random 60)))
             (setq wpn-name "Shotgun"))
           (progn (setq can-fire nil) (log-msg "Shotgun out of shells! *CLICK*"))))
      ((= *player-weapon* 4)
       ;; BFG 9000
       (if (>= *player-ammo-cells* 20)
           (progn
             (setq *player-ammo-cells* (- *player-ammo-cells* 20))
             (setq dmg (+ 200 (random 150)))
             (setq wpn-name "BFG 9000"))
           (progn (setq can-fire nil) (log-msg "BFG out of energy cells! *CLICK*")))))

    (when can-fire
      ;; Raycast target from center screen
      (let* ((target nil)
             (min-dist 999.0)
             (dir-x (:math:cos *player-angle*))
             (dir-y (:math:sin *player-angle*)))

        (dolist (s *sprites*)
          (when (and (not (eq (getf s :state) :dead))
                     (member (getf s :type) '(:imp :zombie :caco :barrel)))
            (let* ((sx (getf s :x))
                   (sy (getf s :y))
                   (dx (- sx *player-x*))
                   (dy (- sy *player-y*))
                   (d (dist2 *player-x* *player-y* sx sy))
                   ;; Angle to sprite
                   (spr-ang (normalize-angle (:math:atan2 dy dx)))
                   (ang-diff (abs (- spr-ang *player-angle*))))
              (when (> ang-diff *pi*)
                (setq ang-diff (- *two-pi* ang-diff)))
              ;; Check if within aiming cone (~15 deg)
              (when (and (< ang-diff 0.25) (< d min-dist))
                (setq min-dist d)
                (setq target s)))))

        (if target
            (let* ((stype (getf target :type))
                   (cur-hp (getf target :hp))
                   (new-hp (- cur-hp dmg)))
              (setf (getf target :hp) new-hp)
              (setf (getf target :alert) 10) ;; Alert target
              (if (<= new-hp 0)
                  (progn
                    (setf (getf target :state) :dead)
                    (if (eq stype :barrel)
                        (progn
                          (log-msg "BOOM! Explosive Barrel detonated!")
                          ;; Detonation AoE
                          (explode-barrel (getf target :x) (getf target :y)))
                        (progn
                          (setq *player-kills* (1+ *player-kills*))
                          (log-msg (format nil "~A blasted ~A! SLAIN!" wpn-name stype)))))
                  (log-msg (format nil "~A hit ~A for ~A damage! (HP: ~A)"
                                   wpn-name stype dmg new-hp))))
            (log-msg (format nil "~A fired into the darkness." wpn-name)))))))

(defun explode-barrel (bx by)
  ;; Damage nearby monsters and player
  (let ((pd (dist2 *player-x* *player-y* bx by)))
    (when (< pd 4.0)
      (let ((p-dmg (floor (* 50.0 (- 1.0 (/ pd 4.0))))))
        (setq *player-hp* (max 0 (- *player-hp* p-dmg)))
        (log-msg (format nil "Caught in barrel blast! -~A HP" p-dmg)))))
  (dolist (s *sprites*)
    (when (and (not (eq (getf s :state) :dead))
               (member (getf s :type) '(:imp :zombie :caco)))
      (let ((sd (dist2 (getf s :x) (getf s :y) bx by)))
        (when (< sd 4.0)
          (let ((sdmg (floor (* 80.0 (- 1.0 (/ sd 4.0))))))
            (let ((new-hp (- (getf s :hp) sdmg)))
              (setf (getf s :hp) new-hp)
              (when (<= new-hp 0)
                (setf (getf s :state) :dead)
                (setq *player-kills* (1+ *player-kills*))))))))))

;; --- Monster AI & Behavior ---
(defun update-monsters ()
  (dolist (s *sprites*)
    (when (and (not (eq (getf s :state) :dead))
               (member (getf s :type) '(:imp :zombie :caco)))
      (let* ((sx (getf s :x))
             (sy (getf s :y))
             (d (dist2 *player-x* *player-y* sx sy))
             (stype (getf s :type))
             (alert (getf s :alert)))

        ;; Check if player is visible / in range
        (when (or (> alert 0) (< d 8.0))
          ;; Alerted! Move towards player
          (let* ((dx (/ (- *player-x* sx) (max d 0.001)))
                 (dy (/ (- *player-y* sy) (max d 0.001)))
                 (speed (if (eq stype :caco) 0.35 0.25))
                 (nx (+ sx (* dx speed)))
                 (ny (+ sy (* dy speed)))
                 (cell (get-map-cell (floor nx) (floor ny))))
            ;; Move if not colliding with wall
            (unless (is-wall-solid cell)
              (setf (getf s :x) nx)
              (setf (getf s :y) ny)))

          ;; Attack player if close enough
          (let ((attack-range (if (eq stype :caco) 5.0 (if (eq stype :imp) 4.0 1.5))))
            (when (< d attack-range)
              (let ((raw-dmg (if (eq stype :caco) (+ 12 (random 12))
                                 (if (eq stype :imp) (+ 8 (random 8))
                                     (+ 5 (random 5))))))
                ;; Armor absorption
                (let ((armor-absorbed (min *player-armor* (floor (/ raw-dmg 2.0)))))
                  (setq *player-armor* (- *player-armor* armor-absorbed))
                  (let ((hp-loss (- raw-dmg armor-absorbed)))
                    (setq *player-hp* (max 0 (- *player-hp* hp-loss)))
                    (setq *flash-type* :hit)
                    (setq *player-face* ">_<")
                    (setq *player-face-timer* 2)
                    (log-msg (format nil "~A attacks you for ~A damage! (HP: ~A)"
                                     stype hp-loss *player-hp*))))))))))))

;; --- Game Loop & Keyboard Input ---
(defparameter *input-port* nil)

(defun init-input-port ()
  (setq *input-port*
        (ignore-errors
          (Port.open
            (:erlang:list_to_tuple (list :spawn "sh -c \"exec 2>/dev/null; (stty -echo cbreak opost onlcr < /dev/tty || stty -F /dev/tty -echo cbreak opost onlcr || stty raw -echo opost onlcr < /dev/tty || stty -F /dev/tty raw -echo opost onlcr); exec cat < /dev/tty\""))
            (list :binary)))))

(defun close-input-port ()
  (when *input-port*
    (ignore-errors (Port.close *input-port*))
    (setq *input-port* nil))
  (ignore-errors (System.cmd "sh" ["-c" "exec 2>/dev/null; (stty sane < /dev/tty || stty -F /dev/tty sane) || true"])))

(defun read-raw-input ()
  (let ((got nil))
    (when *input-port*
      (let ((msg (receive-msg 50 nil)))
        (when (and msg (Kernel.is_tuple msg))
          (let ((data-tuple (Kernel.elem msg 1)))
            (when (and data-tuple (Kernel.is_tuple data-tuple) (eq (Kernel.elem data-tuple 0) :data))
              (setq got (Kernel.elem data-tuple 1)))))))
    (if got
        got
        ;; Fallback when port is unavailable
        (let ((input (IO.getn "" 1)))
          (when (string= input (string *esc*))
            (let ((next1 (IO.getn "" 1)))
              (when (or (string= next1 "[") (string= next1 "O"))
                (let ((next2 (IO.getn "" 1)))
                  (setq input (format nil "~A~A~A" *esc* next1 next2))))))
          input))))

(defun parse-key-input (raw)
  (let ((key (if (and (stringp raw) (> (length raw) 0)) raw "")))
    (cond
      ((or (string= key "w") (string= key "W")
           (string= key (format nil "~A[A" *esc*))
           (string= key (format nil "~AOA" *esc*))) :up)
      ((or (string= key "s") (string= key "S")
           (string= key (format nil "~A[B" *esc*))
           (string= key (format nil "~AOB" *esc*))) :down)
      ((or (string= key "a") (string= key "A")) :strafe-left)
      ((or (string= key "d") (string= key "D")) :strafe-right)
      ((or (string= key "q") (string= key "Q")
           (string= key (format nil "~A[D" *esc*))
           (string= key (format nil "~AOD" *esc*))) :turn-left)
      ((or (string= key "e") (string= key "E")
           (string= key (format nil "~A[C" *esc*))
           (string= key (format nil "~AOC" *esc*))) :turn-right)
      ((or (string= key " ") (string= key "f") (string= key "F")) :fire)
      ((string= key "1") :weapon-1)
      ((string= key "2") :weapon-2)
      ((string= key "3") :weapon-3)
      ((string= key "4") :weapon-4)
      ((or (string= key "m") (string= key "M")) :map)
      ((or (string= key "x") (string= key "X") (string= key "exit") (string= key (string (code-char 3)))) :quit)
      (t :none))))

(defun handle-player-action (action)
  (let ((move-speed 0.5)
        (rot-speed 0.15))
    ;; Reset flash if timed out
    (when (> *flash-timer* 0)
      (setq *flash-timer* (1- *flash-timer*))
      (when (= *flash-timer* 0) (setq *flash-type* nil)))

    (when (> *player-face-timer* 0)
      (setq *player-face-timer* (1- *player-face-timer*))
      (when (= *player-face-timer* 0)
        (setq *player-face* (if (< *player-hp* 30) "•д•" "^_^"))))

    (cond
      ((eq action :up)
       (let ((dx (* (:math:cos *player-angle*) move-speed))
             (dy (* (:math:sin *player-angle*) move-speed)))
         (try-move-player dx dy)))

      ((eq action :down)
       (let ((dx (* (- (:math:cos *player-angle*)) move-speed))
             (dy (* (- (:math:sin *player-angle*)) move-speed)))
         (try-move-player dx dy)))

      ((eq action :strafe-left)
       (let ((dx (* (:math:sin *player-angle*) move-speed))
             (dy (* (- (:math:cos *player-angle*)) move-speed)))
         (try-move-player dx dy)))

      ((eq action :strafe-right)
       (let ((dx (* (- (:math:sin *player-angle*)) move-speed))
             (dy (* (:math:cos *player-angle*) move-speed)))
         (try-move-player dx dy)))

      ((eq action :turn-left)
       (setq *player-angle* (normalize-angle (- *player-angle* rot-speed))))

      ((eq action :turn-right)
       (setq *player-angle* (normalize-angle (+ *player-angle* rot-speed))))

      ((eq action :fire)
       (fire-weapon))

      ((eq action :weapon-1)
       (setq *player-weapon* 1)
       (log-msg "Equipped Fist"))

      ((eq action :weapon-2)
       (setq *player-weapon* 2)
       (log-msg "Equipped Pistol"))

      ((eq action :weapon-3)
       (if *has-shotgun*
           (progn (setq *player-weapon* 3) (log-msg "Equipped Shotgun"))
           (log-msg "You don't have a Shotgun!")))

      ((eq action :weapon-4)
       (if *has-bfg*
           (progn (setq *player-weapon* 4) (log-msg "Equipped BFG 9000!"))
           (log-msg "You don't have the BFG 9000!")))

      ((eq action :map)
       (setq *show-minimap* (not *show-minimap*))
       (log-msg (if *show-minimap* "Automap ON" "Automap OFF"))))))

;; --- Title Screen Banner ---
(defun show-title-screen ()
  (clear-screen)
  (let ((banner
         ["=========================================================================="
          "  ██████╗██╗     ██████╗  ██████╗  ██████╗ ███╗   ███╗"
          " ██╔════╝██║     ██╔══██╗██╔═══██╗██╔═══██╗████╗ ████║"
          " ██║     ██║     ██║  ██║██║   ██║██║   ██║██╔████╔██║"
          " ██║     ██║     ██║  ██║██║   ██║██║   ██║██║╚██╔╝██║"
          " ╚██████╗███████╗██████╔╝╚██████╔╝╚██████╔╝██║ ╚═╝ ██║"
          "  ╚═════╝╚══════╝╚═════╝  ╚═════╝  ╚═════╝ ╚═╝     ╚═╝"
          "=========================================================================="
          "               EXLISP RAYCASTING 3D FIRST PERSON SHOOTER                  "
          "=========================================================================="
          ""
          "  [1] START NEW GAME"
          "  [2] SELECT EPISODE / LEVEL"
          "  [Q] QUIT"
          ""]))
    (dotimes (i (length banner))
      (let ((line (elt banner i)))
        (put-line (ansi-color (if (< i 8) "31;1" "97;1") line))))))

;; --- Main Game Entry Point ---
(defun run-doom ()
  (init-input-port)
  (hide-cursor)
  (load-level 0)
  (show-title-screen)
  (IO.write (ansi-color "93;1" "Press ANY KEY to start >> "))
  (read-raw-input)
  (clear-screen)

  (let ((running t))
    (loop while running do
      ;; Render current frame
      (render-view)

      ;; Check Death
      (when (<= *player-hp* 0)
        (setq *player-face* "x_x")
        (render-view)
        (put-line (ansi-color "41;97;1" "\n\n   === YOU HAVE DIED ===   \n"))
        (put-line (ansi-color "97;1" "The forces of Hell have overwhelmed you...\n"))
        (setq running nil))

      ;; Check Level Clear
      (when *level-cleared*
        (if (< (1+ *current-level-index*) (length *maps*))
            (progn
              (put-line (ansi-color "42;97;1" "\n\n   === LEVEL COMPLETED! ===   \n"))
              (load-level (1+ *current-level-index*))
              (clear-screen))
            (progn
              (put-line (ansi-color "42;97;1" "\n\n   === VICTORY! ALL DEMONS PURGED! ===   \n"))
              (put-line (ansi-color "93;1" "You conquered Phobos and stopped the invasion!\n"))
              (setq running nil))))

      ;; If still running, get input
      (when (and running (<= *player-hp* 0))
        (setq running nil))

      (when running
        (let ((input (read-raw-input)))
          (let ((action (parse-key-input input)))
            (if (eq action :quit)
                (setq running nil)
                (progn
                  (handle-player-action action)
                  ;; Update Monster AI if player acted
                  (update-monsters))))))))

  ;; Cleanup & Restore terminal
  (close-input-port)
  (show-cursor)
  (put-line (ansi-color "32;1" "\nThank you for playing CL-DOOM on ExLisp!\n")))

;; Auto-start when executed as script
(run-doom)

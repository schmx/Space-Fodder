;;; -*- mode: lisp; indent-tabs: nil -*-



;; TODO add the viewport to the screendrawing thingie
;; TODO translate screen coords -> world coords
;; TODO translate world coords -> screen coords


;; TODO using-location macro?

(defpackage space-fodder
  (:use :cl :sheeple :until-it-dies)
  (:shadowing-import-from :uid :step)
  (:export test))

(in-package :space-fodder)

(defvar *resource-directory*
  "/home/marcus/src/clbuild/source/space-fodder/res/")

(defparameter *window-width* 600)
(defparameter *window-height* 600)

(defparameter *space-fodder*
  (create-engine :title "SPACE F0DDER"
		 :mouse-visible-p nil
		 :resizablep nil
		 :window-width *window-width*
		 :window-height *window-height*))

(defproto =map-tile= ()
  (content
   passablep))

(defvar game-map
  (make-array '(100 100) :element-type 'object))

(defproto =viewport= ()
  ((x 0)
   (y 0)
   (width *window-width*)
   (height *window-height*)))

(defproto =space-object= ()
  ((x nil)
   (y nil)
   (rotation nil)
   content))

(defproto =moving-object= =space-object=
  ((destination nil)
   (velocity nil)))

(defreply draw ((entity =space-object=)
		&rest args &key)
  (with-properties (x y content)
    entity
    (apply 'draw-at x y content args)))

(defproto *pointer* =space-object=
  ;; Pointer used during game play.
  ;; TODO: Add some frames for dem variations.
  ((content (create-image (merge-pathnames "gamepointer.png" *resource-directory*)))
   (x 300)
   (y 300)))

(defproto *our-hero* =moving-object=
  ;; TODO merge-pathname etc.
  ((content (create-image #P"/home/marcus/src/clbuild/source/space-fodder/res/ourhero.png"))
   (x 200)
   (y 200)
   (destination nil)
   (velocity nil)
   (rotation 0)
   (engine 10)  ; PIXELS / SECOND, innit
   ))

(defreply draw ((engine *space-fodder*)
		&key)
  (draw *our-hero* :rotation (rotation *our-hero*))
  (draw *pointer*))

(defun it-rotates-in-space (space-sheep new-x new-y)
  "Calculate rotation for pointing SPACE-SHEEP at (NEW-X,NEW-Y)"
  (with-properties (x y)
    space-sheep
    
    (uid::radians->degrees (atan (- new-x x)
				 (- new-y y)))))

(defun in-range (value range-start range-end)
  (and (> value range-start)
       (< value range-end)))

(defun going-somewhere-p (space-sheep)
  "Is SPACE-SHEEP going somewhere?"
  ;; TODO: this machine is broken
  (not (equal (first (destination space-sheep)) 0)))

(defun it-moves-in-space (space-sheep dt)
  "Set some new X and Y position for SPACE-SHEEP."
  (when (going-somewhere-p space-sheep)
    ))

(defun it-velocites-about (space-sheep)
  "Calculate new velocity vector for SPACE-SHEEP."
  (with-properties (velocity destination x y engine)
    space-sheep
    (let* ((dxen     (- (first destination)
			x))
	   (dyen     (- (second destination)
			y))
	   (distance (sqrt (+ (* 2 dxen)
			      (* 2 dyen))))
	   (time     (/ distance engine)))
      ;; Calculate distance needed to travel and
      ;; use this to look up time. Time is used as
      ;; scaling factor for velocity vector.
      (setf (first velocity)  (/ dxen time)
	    (second velocity) (/ dyen time)))))

(defreply mouse-move :after ((engine *space-fodder*) x y)
  (with-properties ((pointer-x x)
		    (pointer-y y))
    *pointer*
    ;; move the pointer
    (setf pointer-x x
	  pointer-y y)
    (with-properties ((rotation rotation)) *our-hero*
      ;; set rotation for our hero
      (setf rotation (it-rotates-in-space *our-hero* 
					  pointer-x
					  pointer-y)))))

(defreply mouse-down ((engine *space-fodder*) button)
  (declare (ignore button))
  (with-properties ((mouse-x mouse-x)
		    (mouse-y mouse-y))
    *space-fodder*
    (with-properties ((destination destination)
		      (velocity velocity)
		      (hero-engine engine)
		      (rotation rotation))
      *our-hero*
      #+nil(setf (x destination) mouse-x
	    (y destination) mouse-y
	    ;; TODO : velocity vectory!
	    ;; 07:06 < Ralith> schme: well, newposition - position is the line you'll have to traverse to get to it.
	    ;; 07:07 < Ralith> divide that by the amount of time you want to take getting there and you have a velocity.
	    ;; (200 200) => (230 250)  => (30 50)
	    ;; translate (30 50) to actual distance to check time it will take (pix/sec)
	    ;; and scale vector
	    velocity (list )))))

(defreply update ((entity *our-hero*)
		  dt &key)
  (declare (ignore dt))
  (it-moves-in-space *our-hero* dt)
  ;; Q: how often is this update engine thing called?
  ;; A: variable. use dt (deltatime) for figuring stuff out.
  ;; I like to move it, move it.
  ;; TODO: move this sucker
  ;; TODO: accel, deaccel in movement

  )



(defreply update ((engine *space-fodder*)
		  dt &key)
  (update *our-hero* dt))

(defun space-fodder ()
  (run *space-fodder*))

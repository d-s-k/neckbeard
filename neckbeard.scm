(use-modules
 ;; list library
 (srfi srfi-1)
 ;; receive
 (srfi srfi-8)
 ;; define-record-type
 (srfi srfi-9)
 ;; vector library
 (srfi srfi-43))

;;; Diatonic pitches and intervals
;;;


(define diatonic-pattern
  ;;      T T S T T T S
  (vector 2 2 1 2 2 2 1))

(define diatonic-names
  (vector 'c 'd 'e 'f 'g 'a 'b))

;;; Per convention, diatonic numbers are 1-indexed and pitch-classes
;;; are 0-indexed.
(define (diatonic-name number)
  (vector-ref diatonic-names (- number 1)))

(define (diatonic-number name)
  (+ 1 (vector-index (lambda (x) (eq? x name)) diatonic-names)))

(define diatonic-semitones
  (let ((vec (make-vector 7)))
    (do ((i 0 (+ i 1))
         (sum 0 (+ sum (vector-ref diatonic-pattern i))))
        ((= i 7) vec)
      (vector-set! vec i sum))))

(define (diatonic-interval-semitones n)
  (receive (interval octaves) (compound->simple n)
    (+ (vector-ref diatonic-semitones (- interval 1))
       (* 12 octaves))))

;;; Pitch classes
;;;


(define flat -1)
(define sharp 1)
(define double-flat -2)
(define double-sharp 2)

(define (mod12 x) (modulo x 12))
(define mod12+ (compose mod12 +))
(define mod12- (compose mod12 -))

(define (pitch-class name . accidentals)
  (mod12+ (diatonic-interval-semitones (diatonic-number name))
          (apply + accidentals)))

;;; Intervals
;;;


(define-record-type <interval>
  (make-interval quality number semitones)
  interval?
  (quality interval-quality)
  (number interval-number)
  (semitones interval-semitones))

(define (compound->simple n)
  (receive (octaves interval) (floor/ n 7)
    (if (zero? interval)
        (values 7 (- octaves 1))
        (values interval octaves))))

(define (compound->simple/preserved-octaves n)
  (receive (octaves interval) (floor/ n 7)
    (cond
     ((zero? interval)
      (values 7 (- octaves 1)))
     ((and (= 1 interval) (> octaves 0))
      (values 8 (- octaves 1)))
     (else
      (values interval octaves)))))

(define (make-interval-maker quality-name degrees offsets)
  (lambda (degree)
    (receive (interval octaves) (compound->simple degree)
      (let loop ((degrees degrees) (offsets offsets))
        (cond ((null? degrees)
               (error "invalid interval" quality-name degree))
              ((= interval (car degrees))
               (make-interval quality-name
                              degree
                              (+ (car offsets)
                                 (diatonic-interval-semitones interval)
                                 (* 12 octaves))))
               (else
                (loop (cdr degrees) (cdr offsets))))))))

(define major
  (make-interval-maker 'major
                       '(2 3 6 7)
                       '(0 0 0 0)))

(define minor
  (make-interval-maker 'minor
                       '( 2  3  6  7)
                       '(-1 -1 -1 -1)))

(define perfect
  (make-interval-maker 'perfect
                       '(1 4 5)
                       '(0 0 0)))

(define augmented
  (make-interval-maker 'augmented
                       '(1 2 3 4 5 6 7)
                       '(1 1 1 1 1 1 1)))

(define diminished
  (make-interval-maker 'diminished
                       '( 1  2  3  4  5  6  7)
                       '(-1 -2 -2 -1 -1 -2 -2)))

;;; Chords
;;;


(define-record-type <chord>
  (make-chord root-letter alteration intervals)
  chord?
  (root-letter chord-root-letter)
  (alteration chord-alteration)
  (intervals chord-intervals))

(define (make-chord-maker . intervals)
  (lambda (root-letter . accidentals)
    (make-chord root-letter
                (apply + accidentals)
                intervals)))

(define major-triad
  (make-chord-maker (major 3) (perfect 5)))
(define minor-triad
  (make-chord-maker (minor 3) (perfect 5)))
(define augmented-triad
  (make-chord-maker (major 3) (augmented 5)))
(define diminished-triad
  (make-chord-maker (minor 3) (diminished 5)))
(define suspended-second
  (make-chord-maker (major 2) (perfect 5)))
(define suspended-fourth
  (make-chord-maker (perfect 4) (perfect 5)))
(define major-seventh
  (make-chord-maker (major 3) (perfect 5) (major 7)))
(define minor-seventh
  (make-chord-maker (minor 3) (perfect 5) (minor 7)))
(define major-minor-seventh             ; AKA dominant seventh
  (make-chord-maker (major 3) (perfect 5) (minor 7)))
(define major-ninth
  (make-chord-maker (major 3) (perfect 5) (major 7) (major 9)))
(define minor-ninth
  (make-chord-maker (minor 3) (perfect 5) (minor 7) (minor 9)))
(define dominant-minor-ninth
  (make-chord-maker (major 3) (perfect 5) (minor 7) (minor 9)))

(define (describe-alteration alteration)
  (case alteration
    ((0)  '())
    ((-1) '(flat))
    ((1)  '(sharp))
    ((-2) '(double-flat))
    ((2)  '(double-sharp))
    ((-3) '(triple-flat))
    ((3)  '(triple-sharp))
    (else
     (make-list (abs alteration)
                (if (negative? alteration) 'flat 'sharp)))))

(define (chord-pitch-classes chord)
  (let ((root-pitch-class (pitch-class (chord-root-letter chord)
                                       (chord-alteration chord))))
    (cons root-pitch-class
          (map (lambda (interval)
                 (mod12+ root-pitch-class (interval-semitones interval)))
               (chord-intervals chord)))))

(define (chord-diatonic-numbers chord)
  (let ((root-diatonic-number (diatonic-number (chord-root-letter chord)))
        (interval-numbers (map interval-number (chord-intervals chord))))
    (cons root-diatonic-number
          (map (lambda (interval-number)
                 (compound->simple (+ root-diatonic-number
                                      (- interval-number 1))))
               interval-numbers))))

(define (chord-constituent-alterations chord)
  (let* ((pitch-classes (chord-pitch-classes chord))
         (root-pitch-class (car pitch-classes))
         (root-diatonic-number (diatonic-number (chord-root-letter chord)))
         (actual-semitones
          (map (lambda (pitch-class)
                 (mod12- pitch-class root-pitch-class))
               pitch-classes))
         (diatonic-semitones
          (map (lambda (n)
                 (mod12- (diatonic-interval-semitones n)
                         (diatonic-interval-semitones root-diatonic-number)))
               (chord-diatonic-numbers chord))))
    (map (lambda (actual diatonic)
           (+ (- actual diatonic) (chord-alteration chord)))
         actual-semitones
         diatonic-semitones)))

(define (spell-chord chord)
  (map (lambda (name alteration)
         (if (null? alteration)
             name
             (cons name alteration)))
       (map diatonic-name (chord-diatonic-numbers chord))
       (map describe-alteration (chord-constituent-alterations chord))))

;;; Chord diagrams
;;;


(define-record-type <chord-diagram>
  (%make-chord-diagram nstrings rows skipped-strings lowest-fret highest-fret)
  chord-diagram?
  (nstrings chord-diagram-nstrings)
  (rows chord-diagram-rows)
  (skipped-strings chord-diagram-skipped-strings)
  (lowest-fret chord-diagram-lowest-fret)
  (highest-fret chord-diagram-highest-fret))

(define (make-vector/thunk length thunk)
  (vector-unfold (lambda (i x) (values (thunk) i))
                 length
                 #f))

;;; A chord fingering is a list of fret positions with #f indicating a
;;; string that does not sound.
(define (make-chord-diagram chord-fingering)
  ;; -> vector of row lists, lowest non-zero fret position or zero if
  ;; all sounding strings are played open, highest fret position
  (define nstrings (length chord-fingering))
  (define (index-fingering chord-fingering)
    (map cons chord-fingering (iota nstrings)))
  (define (consolidate indexed-fingering nrows)
    (let ((buckets (make-vector/thunk nrows (lambda () (list))))
          (skipped-strings (list)))
      (for-each (lambda (fingering)
                  (let ((i (car fingering))
                        (string (cdr fingering)))
                    (if i
                        (vector-set! buckets
                                     i
                                     (if string
                                         (cons (cdr fingering)
                                               (vector-ref buckets i))
                                         '()))
                        (set! skipped-strings
                          (cons string skipped-strings)))))
                indexed-fingering)
      (values buckets skipped-strings)))
  (define (max* l) (if (null? l) 0 (apply max l)))
  (define (min* l) (if (null? l) 0 (apply min l)))
  (let* ((chord-fingering-fretted-strings
          (filter (lambda (x) (and (integer? x) (positive? x)))
                  chord-fingering))
         (highest-fret
          (max* chord-fingering-fretted-strings))
         (lowest-fret
          (min* chord-fingering-fretted-strings))
         (empty-frets
          (map (lambda (x) (cons x #f))
               (iota (+ highest-fret 1)))))
    (receive (rows skipped-strings)
        (consolidate
         (sort (lset-union (lambda (a b) (eq? (car a) (car b)))
                           (index-fingering chord-fingering)
                           empty-frets)
               (lambda (a b)
                 (let ((x (car a)) (y (car b)))
                   (and (integer? x)
                        (integer? y)
                        (< x y)))))
         (+ highest-fret 1))
      (%make-chord-diagram
       nstrings rows skipped-strings lowest-fret highest-fret))))

(define string-char (make-parameter #\|))
(define fretted-char (make-parameter #\@))
(define open-string-char (make-parameter #\O))
(define skipped-string-char (make-parameter #\X))
(define horizontal-divider-char (make-parameter #\-))

(define spaces-between-strings (make-parameter 1))
(define lowest-fret-label (make-parameter 4))

(define (left-margin chord-diagram row)
  (let* ((highest-fret (chord-diagram-highest-fret chord-diagram))
         (spacing-after-label (if (< highest-fret 10) "  " " ")))
    (if (and (= row highest-fret) (>= row (lowest-fret-label)))
        (call-with-output-string
          (lambda (port)
            (write highest-fret port)
            (display spacing-after-label port)))
        "   ")))

(define (make-row-string chord-diagram fill-char)
  (let ((nstrings (chord-diagram-nstrings chord-diagram)))
    (make-string (- (+ nstrings (* nstrings (spaces-between-strings)))
                          1)
                 fill-char)))

(define* (fill-columns! string positions char)
  (for-each (lambda (position)
              (string-set! string
                           (* position (+ 1 (spaces-between-strings)))
                           char))
            positions)
  string)

(define (render-top-row chord-diagram)
  (let ((top-row-string (make-row-string chord-diagram #\space))
        (open-strings (vector-ref (chord-diagram-rows chord-diagram) 0))
        (skipped-strings (chord-diagram-skipped-strings chord-diagram)))
    (unless (null? open-strings)
      (fill-columns! top-row-string open-strings (open-string-char)))
    (unless (null? skipped-strings)
      (fill-columns! top-row-string skipped-strings (skipped-string-char)))
    top-row-string))

(define (render-row chord-diagram n)
  (let ((row-list (vector-ref (chord-diagram-rows chord-diagram) n))
        (row-string (make-row-string chord-diagram #\space)))
    (fill-columns! row-string
                   (iota (chord-diagram-nstrings chord-diagram))
                   (string-char))
    (unless (null? row-list)
      (fill-columns! row-string
                     row-list
                     (fretted-char)))
    row-string))

(define (chord-diagram-render-text chord-diagram)
  (let ((horizontal-divider (make-row-string chord-diagram
                                             (horizontal-divider-char)))
        (lowest-fret (chord-diagram-lowest-fret chord-diagram))
        (highest-fret (chord-diagram-highest-fret chord-diagram)))
    (call-with-output-string
      (lambda (port)
        (display (left-margin chord-diagram 0) port)
        (display (render-top-row chord-diagram) port)
        (newline port)
        (display (left-margin chord-diagram 0) port)
        (display horizontal-divider port)
        (newline port)
        (when (> highest-fret 0)
          (do ((i lowest-fret (+ i 1)))
              ((> i highest-fret))
            (display (left-margin chord-diagram i) port)
            (display (render-row chord-diagram i) port)
            (newline port)))))))

;; (display (chord-diagram-render-text (make-chord-diagram '(0 7 6 4 4 #f))))
;;
;;    O         X
;;    -----------
;;    | | | @ @ |
;;    | | | | | |
;;    | | @ | | |
;; 7  | @ | | | |

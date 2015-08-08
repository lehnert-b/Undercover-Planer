#lang racket/gui

; Copyright 2015, Bernhard Lehnert, Greifswald, Germany
; Dieses Programm steht unter der General Public Licence Version 3 GPLv3
; Undercover Planer ist Freie Software: Sie können es unter den Bedingungen
; der GNU General Public License, wie von der Free Software Foundation,
; Version 3 der Lizenz oder (nach Ihrer Wahl) jeder neueren
; veröffentlichten Version, weiterverbreiten und/oder modifizieren.
;
; Dieses Programm wird in der Hoffnung, dass es nützlich sein wird, aber
; OHNE JEDE GEWÄHRLEISTUNG, bereitgestellt; sogar ohne die implizite
; Gewährleistung der MARKTFÄHIGKEIT oder EIGNUNG FÜR EINEN BESTIMMTEN ZWECK.
; Siehe die GNU General Public License für weitere Details.
;
; Sie sollten eine Kopie der GNU General Public License zusammen mit diesem
; Programm erhalten haben. Wenn nicht, siehe <http://www.gnu.org/licenses/>.


; Drawing Logic
(define style-pen (new pen% [style 'solid][cap 'butt]))
(define drill-pen (new pen% [style 'transparent]))
(define drill-brush (new brush% [color (make-color 255 255 255 0.75)]))
(define wood1-pen (new pen% [color "grey"] [width 0]))
(define wood1-brush (new brush% [color "Orange"]))
(define wood2-pen (new pen% [color "grey"]))
(define wood2-brush (new brush% [color "SandyBrown"]))
(define screw-pen (new pen% [style 'transparent]))
(define screw-brush (new brush% [color "black"]))

(define text-settings (lambda () (string-append "Gebohrtes Brett: " (send upper-thickn get-value)
                                              "mm\nAnderes Brett: " (send upper-thickn get-value)
                                              "mm\nWinkel: " (send angle get-value) "°\n"
                                              "Raste am Undercover-Jig Nr. " (number->string(+ 1 (send jig-setting get-selection)))
                                              "\nAnschlag: " (if (equal? 0 (send jig-angle get-selection)) "abgewinkelt" "gerade/flach")
                                              "\nBohrerlänge: " (send drill-length get-value) "mm\nSchraubenlänge: "
                                              (send screw-length get-value) "mm\n")))

(define (get-val widget)
  (define intermediate ;\
    (string->number (string-replace (string-trim (send widget get-value)) "," ".")))
  (if intermediate intermediate 0))

(define (draw-scale dc)
  (send dc set-pen style-pen)
  (send dc draw-line -97 -97 -97 (+ -97 50))
  (for([i (in-range 6)])
    (send dc draw-line -99 (+ -97 (* i 10)) -95 (+ -97 (* i 10)))))

(define (draw-lower-board dc)
  (send dc set-brush wood1-brush)
  (send dc draw-rectangle -97 0 (* 2 97) (get-val lower-thickn)))

(define (draw-upper-board dc)
  (send dc set-brush wood2-brush)
  (define thick (get-val upper-thickn))
  (define radians (* 0.0174533 (- 90 (get-val angle)) ))
  (send dc rotate radians);
  (define upper-board-path (new dc-path%))
  (send upper-board-path move-to (* -1 thick) -97)
  (send upper-board-path line-to 0 -97)
  (send upper-board-path line-to 0 0)
  (send upper-board-path line-to (* -1 thick) (* (tan (* -1 radians)) (get-val upper-thickn)))
  (send upper-board-path move-to (* -1 thick) -97)
  (send dc draw-path upper-board-path)
  (send dc rotate (* -1 radians)))

(define (draw-hole-etc dc)
  (send dc set-brush drill-brush)
  (send dc set-pen drill-pen)
  (define radians (* 0.0174533 (- 90 (get-val angle)) ))   ; degree to radians conversion
  (send dc rotate radians)                                 ; rotate upper board if necessary 
  (define height-nr (send jig-setting get-selection))      ; hight item as specified in GUI
  (define height1 (first(drop '(-23 -31 -36 -63) height-nr))); hier Liste der Abstände hinterlegen
  (define height2 (- height1 18.353))  ; Korrektur für Mitte des Bohrers
  (define height (if (> (send jig-angle get-selection) 0) height2 (+ height2 5))) ; is jig angled or flat? Makes about 5 mm
  (send dc set-origin 0 height)
  (send dc rotate (* 0.0174533 -15))  ; rotate 15 degrees 
  (define depth (- 82.7 (get-val drill-length)))  ; Bohrerlänge etwa 16,8cm, minus etwa 1cm Anschlagsring minus 75.3 mm im Jig
  (send dc draw-rectangle -4.25 -60 9.5 (+ 60 (if (> depth -50) depth 0)))
  (send dc set-pen screw-pen)
  (send dc set-brush screw-brush)
  (send dc draw-rectangle -2 depth 4 (get-val screw-length))
  (send dc rotate (* 0.0174533 15))
  (send dc set-origin 0 (* -1 height))
  (send dc rotate (* -1 radians)))

(define (draw-it canvas dc)
  (send dc erase)
  (send dc set-smoothing 'smoothed)
  (draw-scale dc)
  (draw-lower-board dc)
  (draw-upper-board dc)
  (draw-hole-etc dc))

(define (draw-all button event)
  (define dc (send my-canvas get-dc))
  (send dc clear )
  (draw-it my-canvas dc))

(define draw-pdf (lambda ()
                   (define dc (new pdf-dc% [parent #f]
                                   [interactive #f]
                                   ))
                   (send dc set-origin 400 300)
                   (send dc set-scale 2.0 2.0)
                   (send dc start-doc "Drucke Plan als PDF")
                   (send dc start-page)
                   (draw-it '() dc)
                   (send dc end-page)
                   (send dc end-doc)))

(define draw-svg (lambda ()
                   (define dc (new svg-dc% [width 500]
                                   [height 500]
                                   [output (put-file "Wohin SVG speichern?")]
                                   [exists 'replace]))
                   (send dc set-origin 100 100)
                   (send dc start-doc "Drucke Plan als SVG")
                   (send dc start-page)
                   (draw-it '() dc)
                   (send dc end-page)
                   (send dc end-doc)))

(define text-clipboard (lambda ()
                         (send the-clipboard set-clipboard-string (text-settings) 0)))

(define licence-info (lambda ()
                      (message-box "(c) 2015 Bernhard Lehnert"
                                   (string-append "Undercover Planer wurde 2015 von Bernhard Lehnert aus Greifswald erstellt und "
                                                  "unter die General Public Licence Version 3 gestellt. Dies bedeutet, dass Sie "
                                                  "das Programm frei kopieren, kostenlos benutzen und weitergeben dürfen. Es "
                                                  "bedeutet auch, dass das Programm ohne jede Garantie kommt und der Autor keine "
                                                  "Haftung übernimmt, wenn Sie z. B. aufgrund eines Programmfehlers ein falsches "
                                                  "Loch bohren.\n"
                                                  "Die ganze Lizenz sollte diesem Programm beiliegen, gültig ist die Version 3 "
                                                  "unter http://www.gnu.org/licenses/gpl.txt\n"
                                                  "\nSie dürfen das Programm kostenlos benutzen, egal ob beruflich oder privat."
                                                  ))))

(define version-info (lambda ()
                      (message-box "Version 0.9"
                                   (string-append "Sie benutzen Version 0.9 von Undercover Planer,\n"
                                                  "veröffentlicht im August 2015. "
                                                 ))))
; GUI definitions
(define frame (new frame% [label "Undercover Planer"]))

(define msg (new message% [parent frame]
                 [label "Copyright 2015 Bernhard Lehnert. GPLv3."]))

(define hpanel (new horizontal-panel% [parent frame]))

(define right-panel (new vertical-panel% [parent hpanel]))

(define my-canvas (new canvas% [parent hpanel]
                       [min-width 400]
                       [min-height 400]
                       [paint-callback draw-it]
                       [style '(no-focus control-border)]
                       ))

(send (send my-canvas get-dc) set-origin 200 200)
(send (send my-canvas get-dc) set-scale 1.8 1.8)


(define upper-thickn (new combo-field% [parent right-panel]
                          [label "Dicke &oberes Brett in [mm]"]
                          [init-value "18"]
                          [choices '("12" "18" "22" "50")]
                          [callback draw-all]))


(define lower-thickn (new combo-field% [parent right-panel]
                          [label "Dicke &unteres Brett in [mm]"]
                          [init-value "18"]
                          [choices '("12" "18" "22" "50")]
                          [callback draw-all]
                          ))

(define angle (new combo-field% [parent right-panel]
                   [label "Winkel [°]"]
                   [init-value "90"]
                   [choices '("150" "135" "120" "90" "60" "45" "30")]
                   [callback draw-all]
                   ))

(define jig-setting (new radio-box% [parent right-panel]
                         [label "Jig-Einstellung"]
                         [choices '("&1" "&2" "&3" "&4")]
                         [style '(horizontal)]
                         [callback draw-all]
                         ))

(define jig-angle(new radio-box% [parent right-panel]
                      [label "Jig-Anschlag"]
                      [choices '("&winkelig" "&flach")]
                      [selection 1]
                      [style '(horizontal)]
                      [callback draw-all]
                      ))

(define drill-length (new combo-field% [parent right-panel]
                          [label "&Bohrerlänge in mm"]
                          [choices '("10" "15" "20" "25" "30" "35" "40")]
                          [init-value "55"]
                          [callback draw-all]
                          ))

(define screw-length (new combo-field% [parent right-panel]
                          [label "&Schraubenlänge in mm"]
                          [choices '("18" "25" "30" "40" "50" "60")]
                          [init-value "18"]
                          [callback draw-all]))


(define menu-bar (new menu-bar% [parent frame]))
(define file-menu (new menu% [parent menu-bar]
                       [label "&File"]))
(define edit-menu (new menu% [parent menu-bar]
                       [label "&Edit"]))
(define about-menu (new menu% [parent menu-bar]
                        [label "&About"]))
(define copy-itrm (new menu-item% [parent edit-menu]
                       [label "Einstellungen in die Zwischenablage"]
                       [callback (lambda (a b) (text-clipboard))]))
(define pdf-item (new menu-item% [parent file-menu]
                      [label "PDF erstellen"]
                      [callback (lambda (x y) (draw-pdf))]))
(define svg-item (new menu-item% [parent file-menu]
                      [label "SVG erstellen"]
                      [callback (lambda (x y) (draw-svg))]))
(define quit-item (new menu-item% [parent file-menu]
                       [label "Quit"]
                       [callback (lambda (x y) (exit 0))]
                       ))
(define licence-item (new menu-item% [parent about-menu][label "Lizenzhinweise"]
                          [callback (lambda (x y) (licence-info))]
                          ))
(define donationware-item (new menu-item% [parent about-menu][label "Version"]
                          [callback (lambda (x y) (version-info))]
                          ))


(send frame show #t)

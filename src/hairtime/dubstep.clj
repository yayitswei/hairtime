(ns hairtime.dubstep
  (:use [overtone.live]))

(defsynth dubstep [bpm 120 wobble 1 note 50 snare-vol 1 kick-vol 1 v 1]
 (let [trig (impulse:kr (/ bpm 120))
       freq (midicps note)
       swr (demand trig 0 (dseq [wobble] INF))
       sweep (lin-exp (lf-tri swr) -1 1 40 3000)
       wob (apply + (saw (* freq [0.99 1.01])))
       wob (lpf wob sweep)
       wob (* 0.8 (normalizer wob))
       wob (+ wob (bpf wob 1500 2))
       wob (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))

       kickenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0 (dseq [1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0] INF))) 0.7)
       kick (* (* kickenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
       kick (clip2 kick 1)

       snare (* 3 (pink-noise [1 1]) (apply + (* (decay (impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
       snare (+ snare (bpf (* 4 snare) 2000))
       snare (clip2 snare 1)]

   (out 0    (* v (clip2 (+ wob (* kick-vol kick) (* snare-vol snare)) 1)))))

(comment
  ;;Control the dubstep synth with the following:
  (dubstep)
  ;=> 25 ; synth ID to use below
  (ctl 28 :wobble 8)
  (ctl 28 :note 40)
  (ctl 28 :bpm 250)
  (stop)
  )
;;
;;
;;
(comment
(demo 30
      (let [trig (coin-gate 0.5 (impulse:kr 2))
            note (demand trig 0 (dseq (shuffle (map midi->hz (conj (range 24 45) 22))) INF))
            sweep (lin-exp (lf-saw (demand trig 0 (drand [1 2 2 3 4 5 6 8 16] INF))) -1 1 40 5000)

            son (mix (lf-saw (* note [0.99 1 1.01])))
            son (lpf son sweep)
            son (normalizer son)
            son (+ son (bpf son 2000 2))

            ;;special flavours
            ;;hi manster
            son (select (< (t-rand:kr :trig trig) 0.05) [son (* 4 (hpf son 1000))])

            ;;sweep manster
            son (select (< (t-rand:kr :trig trig) 0.05) [son (* 4 (hpf son sweep))])

            ;;decimate
            son (select (< (t-rand:kr :trig trig) 0.05) [son (round son 0.1)])

            son (tanh (* son 5))
            son (+ son (* 0.3 (g-verb son 10 0.1 0.7)))
            son (* 0.3 son)]

        [son son]))
)

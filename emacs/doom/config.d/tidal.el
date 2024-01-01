;;; ../.myconf/emacs/doom/config.d/tidal.el -*- lexical-binding: t; -*-

(after! tidal
  (map! :map tidal-mode-map
        :n [return] #'tidal-run-multiple-lines
        :n "1"      #'tidal-run-d1
        :n "s-1"    #'tidal-stop-d1
        :n "2"      #'tidal-run-d2
        :n "s-2"    #'tidal-stop-d2
        :n "3"      #'tidal-run-d3
        :n "s-3"    #'tidal-stop-d3
        :n "4"      #'tidal-run-d4
        :n "s-4"    #'tidal-stop-d4
        :n "5"      #'tidal-run-d5
        :n "s-5"    #'tidal-stop-d5
        :n "6"      #'tidal-run-d6
        :n "s-6"    #'tidal-stop-d6
        :n "7"      #'tidal-run-d7
        :n "s-7"    #'tidal-stop-d7
        :n "8"      #'tidal-run-d8
        :n "s-8"    #'tidal-stop-d8
        :n "9"      #'tidal-run-d9
        :n "s-9"    #'tidal-stop-d9
        :n "§"      #'tidal-hush)
  (map! :leader
        :map tidal-mode-map
        "S" #'tidal-start-haskell))

#!/usr/bin/bb
(ns print
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [babashka.deps :as deps]))

;; etaoin is an external library,
;; so it cannot be added in the :require above,
;; but must go through babashka.deps
;;
;; also requires geckodriver
(deps/add-deps '{:deps {etaoin/etaoin {:mvn/version "1.1.42"}}})
(require '[etaoin.api :as e])

(defn- print-pdf [in out]
  (e/with-firefox driver
    (doto driver
      (e/go (str "file://" in "?print-pdf"))
      (e/wait 3)
      (e/print-page out {:background true}))))

(let [in (fs/absolutize (first *command-line-args*))
      out (-> in fs/strip-ext (str ".pdf"))]
  (when-not (str/ends-with? in "-handout.html")
    (println "Error: incorrect filename or extension! (Must end with '-handout.html')")
    (System/exit 1))
  (print-pdf in out)
  (when (fs/exists? out)
    (fs/delete in))
  (println "PDF print finished successfully!"))

(require 'cljs.repl)
(require 'cljs.build.api)
(require 'cljs.repl.browser)

(cljs.build.api/build "src"
  {:main 'cljs-lab.core
   :output-to "out/main.js"
   :verbose true})

(def env (cljs.repl.browser/repl-env))

(defn reload-on-compile []
  (binding [cljs.repl.browser/browser-state (:browser-state env)
            cljs.repl.browser/ordering (:ordering env)
            cljs.repl.browser/es (:es env)
            cljs.repl.server/state (:server-state env)]
    (cljs.repl.browser/browser-eval "setTimeout(() => location.reload(), 150)")))

(cljs.repl/repl env
  :watch "src"
  :watch-fn reload-on-compile
  :output-dir "out")

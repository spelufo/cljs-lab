(require 'cljs.build.api)

(cljs.build.api/watch "src"
  {:main 'cljs-lab.core
   :output-to "out/main.js"
   :verbose true})
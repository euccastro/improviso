(set-env!
 :source-paths #{"src/clj" "src/cljs" "src/cljc" "src/java"}
 :resource-paths #{"res"}
 :dependencies '[[adzerk/boot-cljs "1.7.228-1" :scope "test"]
                 [adzerk/boot-cljs-repl "0.3.0" :scope "test"]
                 [adzerk/boot-reload "0.4.8" :scope "test"]
                 [pandeiro/boot-http "0.8.3" :scope "test"]
                 [crisptrutski/boot-cljs-test "0.2.2-SNAPSHOT" :scope "test"]
                 [boot-environ "1.0.3"]
                 [clj-time "0.13.0"]
                 ;; using the alpha because that's the version of the API docs
                 ;; in their website.
                 [com.andrewmcveigh/cljs-time "0.5.0-alpha2"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.36"]
                 [compojure "1.4.0"]
                 [org.clojure/core.async "0.2.374"]
                 [datascript "0.16.1"]
                 [com.datomic/datomic-free "0.9.5544" :exclusions [com.google.guava/guava]]
                 [environ "1.0.3"]
                 [http-kit "2.1.19"]  ;; same as used by boot-http
                 [com.cemerick/piggieback "0.2.1" :scope "test"]
                 [ring/ring-defaults "0.1.5"]
                 [rum "0.10.8"]
                 [com.taoensso/sente "1.11.0"]
                 [thi.ng/geom "0.0.1178-SNAPSHOT"]
                 [com.taoensso/timbre "4.7.2"]
                 [org.clojure/tools.nrepl "0.2.12" :scope "test"]
                 [weasel "0.7.0" :scope "test"]])

(require
  '[adzerk.boot-cljs      :refer [cljs]]
  '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
  '[adzerk.boot-reload    :refer [reload]]
  '[environ.boot :refer [environ]]
  '[crisptrutski.boot-cljs-test  :refer [test-cljs]]
  '[pandeiro.boot-http    :refer [serve]])

(deftask auto-test []
  (merge-env! :resource-paths #{"test"})
  (comp (watch)
     (speak)
     (test-cljs)))

(deftask dev []
  (comp (environ :env {:in-development "indeed"})
     (javac)
     (serve :handler 'improviso.core/app
            :resource-root "target"
            :httpkit true
            :reload true)
     (watch)
     (speak)
     (reload :on-jsload 'improviso.core/main
             ;; XXX: make this configurable
             :open-file "emacsclient -n +%s:%s %s")
     (cljs-repl)
     (cljs :source-map true :optimizations :none)
     (target :dir #{"target"})))

(deftask build []
  (comp
   (javac)
   (cljs :optimizations :advanced)
   (aot :namespace '#{improviso.core})
   (pom :project 'improviso
        :version "0.1.0-SNAPSHOT")
   (uber)
   (jar :main 'improviso.core)
   (target :dir #{"target"})))

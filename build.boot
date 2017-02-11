(def project 'pterodactyl)
(def version "0.1.0-SNAPSHOT")

(set-env! :resource-paths #{"src" "test"}
          :source-paths   #{"src"}
          :dependencies   '[[org.clojure/clojure "1.8.0"]
                            [adzerk/boot-test "RELEASE" :scope "test"]
                            [net.cgrand/seqexp "0.6.1"]
                            [it.frbracch/boot-marginalia "0.1.3-1"]])

(task-options!
 pom {:project     project
      :version     version
      :description "Pterodactyl: a programmer's text editor for winged dinosaurs"
      :url         "https://github.com/osfameron/pterodactyl"
      :scm         {:url "https://github.com/osfameron/pterodactyl"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}}
 jar {:main        'pterodactyl.core
      :file        (str "pterodactyl-" version "-standalone.jar")})

(deftask build
  "Build the project locally as a JAR."
  [d dir PATH #{str} "the set of directories to write to (target)."]
  (let [dir (if (seq dir) dir #{"target"})]
    (comp (pom) (uber) (jar) (target :dir dir))))

(deftask run
  "Run the project."
  [a args ARG [str] "the arguments for the application."]
  (require '[pterodactyl.core :as app])
  (apply (resolve 'app/-main) args))

(require '[adzerk.boot-test :refer [test]])
(require '[it.frbracch.boot-marginalia :refer [marginalia]])

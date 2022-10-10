;;; ----------------------------------------------------------------------
;;; Nix defaults
;;; ----------------------------------------------------------------------

(defparameter *nix-zlib* "zlib")

(defparameter *nix-llvm-9* "llvm_9")

(defparameter *nix-curl* "curl")

(defparameter *nix-time* "time")

(defparameter *nix-ldb* "ldb")

;;; ----------------------------------------------------------------------
;;; Dependencies for YAML generation
;;; ----------------------------------------------------------------------

;; --------------------------------------
;; Crypto Style Dependencies
;; --------------------------------------

(defparameter *galois-field*
  (make-dependency-git :name   "https://github.com/serokell/galois-field.git"
                       :commit "576ba98ec947370835a1f308895037c7aa7f8b71"))

(defparameter *elliptic-curve*
  (make-dependency-git :name   "https://github.com/serokell/elliptic-curve.git"
                       :commit "b8a3d0cf8f7bacfed77dc3b697f5d08bd33396a8"))

(defparameter *pairing*
  (make-dependency-git :name   "https://github.com/serokell/pairing.git"
                       :commit "cf86cf1f6b03f478a439703b050c520a9d455353"))

(defparameter *bit-vec*
  (string->dep-sha
   "bitvec-1.0.3.0@sha256:f69ed0e463045cb497a7cf1bc808a2e84ea0ce286cf9507983bb6ed8b4bd3993,3977"))

;; --------------------------------------
;; STM Containers Dependencies
;; --------------------------------------

(defparameter *stm-container*
  (string->dep-sha
   "stm-containers-1.2@sha256:a887f2e7692b7cf20e0b081e2d66e21076e2bd4b57016ec59c484edfa2d29397,3244"))

(defparameter *stm-hamt*
  (string->dep-sha
   "stm-hamt-1.2.0.6@sha256:fba86ccb4b45c5706c19b0e1315ba63dcac3b5d71de945ec001ba921fae80061,3972"))

;; --------------------------------------
;; Tezos Style Dependencies
;; --------------------------------------

(defparameter *tezos-bake-monitor*
  (make-dependency-git :name "https://gitlab.com/obsidian.systems/tezos-bake-monitor-lib.git"
                       :commit "9356f64a6dfc5cf9b108ad84d1f89bcdc1f08174"
                       :subdirs (list "tezos-bake-monitor-lib")))

;; Why do we use such a specific version again
(defparameter *tezos-morley*
  (make-dependency-git :name "https://gitlab.com/morley-framework/morley.git"
                       :commit "53961f48d0d3fb61051fceaa6c9ed6becb7511e5"
                       :subdirs (list "code/morley" "code/morley-prelude")))

;; It seems we were directed to grab these when the system failed to load
(defparameter *morley*
  (make-dependency-git :name "https://gitlab.com/morley-framework/morley.git"
                       :commit "6eb73a0cb8d97039d0706aa3f836cc22fe030f96"
                       :subdirs (list "code/morley")))

(defparameter *morley-prelude*
  (make-dependency-git :name "https://gitlab.com/morley-framework/morley.git"
                       :commit "3584852e68c70cdcf0b346ac9001f8e22f620f35"
                       :subdirs (list "code/morley-prelude")))


(defparameter *base-no-prelude*
  (make-dependency-git :name "https://github.com/serokell/base-noprelude.git"
                       :commit "1282e0b992b00089d55228a2aa9edc4a3581c319")
  "this is the standard version of base no prelude")

;; --------------------------------------
;; Stadnard Library Style Dependencies
;; --------------------------------------

(defparameter *prettiest*
  (make-dependency-github :name "jyp/prettiest"
                          :commit "e5ce6cd6b4da71860c3d97da84bed4a827fa00ef"))


(defparameter *capability*
  (string->dep-sha "capability-0.4.0.0@sha256:d86d85a1691ef0165c77c47ea72eac75c99d21fb82947efe8b2f758991cf1837,3345"))

(defparameter *extensible*
  (make-dependency-github :name "metastatedev/extensible-data"
                          :commit "d11dee6006169cb537e95af28c3541a24194dea8"))

(defparameter *tasty*
  (string->dep-sha "tasty-1.4.1@sha256:69e90e965543faf0fc2c8e486d6c1d8cf81fd108e2c4541234c41490f392f94f,2638"))

(defparameter *aeson-options*
  (string->dep-sha
   "aeson-options-0.1.0@sha256:2d0c25afbb2d038bd5b57de8d042e319ea1a5ec7d7b92810d8a0cf0777882b6a,1244"))

(defparameter *un-exceptionalio*
  (string->dep-sha
   "unexceptionalio-0.5.0@sha256:ad0b2d4d1f62a3e24cdb80360eea42ab3f0a0559af42aba19b5cf373378913ce,1682"))

(defparameter *sr-extra*
  (make-dependency-github :name "seereason/sr-extra"
                          :commit "d5435dcb2ae5da5f9e0fb8e5a3c40f99937a046f"))

;;; ----------------------------------------------------------------------
;;; Groups for YAML generation
;;; ----------------------------------------------------------------------


;; --------------------------------------
;; STM Containers Dependency Groups
;; --------------------------------------
(defparameter *stm-container-group*
  (make-groups :comment "Stm Containers Dependencies"
               :deps (list
                      *stm-container*
                      *stm-hamt*)))

;; --------------------------------------
;; Tezos Dependency Groups
;; --------------------------------------

(defparameter *morley-deps*
  (make-groups :comment "Morley Specific dependencies"
               :deps (list
                      *tezos-bake-monitor*)))

(defparameter *morley-sub-deps*
  (make-groups
   :comment "Git depdencies caused by Morley specific dependencies"
   :deps (list
          *morley*
          *morley-prelude*
          (make-dependency-bare :name "base58-bytestring-0.1.0")
          (make-dependency-bare :name "hex-text-0.1.0.0")
          (string->dep-sha
           "base16-bytestring-0.1.1.7@sha256:0021256a9628971c08da95cb8f4d0d72192f3bb8a7b30b55c080562d17c43dd3,2231")
          (make-dependency-bare :name "show-type-0.1.1")
          (string->dep-sha
           "named-0.3.0.1@sha256:2975d50c9c5d88095026ffc1303d2d9be52e5f588a8f8bcb7003a04b79f10a06,2312")
          (make-dependency-bare :name "cryptonite-0.27")
          (make-dependency-bare :name "uncaught-exception-0.1.0")
          (make-dependency-bare :name "tasty-hunit-compat-0.2.0.1")
          (string->dep-sha
           "with-utf8-1.0.2.2@sha256:42eed140390b3e93d9482b084d1d0150e8774667f39c33bd47e84815751fad09,3057")))
  "this is generic, and used in a few places")

(defparameter *morley-sub-deps-extra*
  (make-groups
   :comment "Git depdencies caused by Morley specific dependencies that are speicific to Michelson"
   :deps (list
          (make-dependency-git :name "https://github.com/int-index/caps.git"
                               :commit "c5d61837eb358989b581ed82b1e79158c4823b1b")
          *base-no-prelude*))
  "like *morley-sub-deps* but is an extra layer of dependency that is not used elsewhere")

(defparameter *morley-deps-testing*
  (make-dependency-git :name "https://gitlab.com/morley-framework/morley.git"
                       :commit "53961f48d0d3fb61051fceaa6c9ed6becb7511e5"
                       :subdirs (list "code/morley" "code/morley-prelude")))

;; --------------------------------------
;; Tezos ∧ Arithmetic Circuit dependcy Groups
;; --------------------------------------

(defparameter *morley-arithmetic-circuit-deps*
  (make-groups :comment "Shared Deps Between Arithmetic Circuits and Morley"
               :deps (list
                      *elliptic-curve*
                      *pairing*
                      *galois-field*)))

(defparameter *sub-morley-arithmetic-circuit-deps*
  (make-groups
   :comment "Sub dependencies of arithmetic-circuit git"
   :deps (list
          (string->dep-sha
           "constraints-extras-0.3.0.2@sha256:bf6884be65958e9188ae3c9e5547abfd6d201df021bff8a4704c2c4fe1e1ae5b,1784")
          (string->dep-sha
           "dependent-sum-0.7.1.0@sha256:5599aa89637db434431b1dd3fa7c34bc3d565ee44f0519bfbc877be1927c2531,2068")
          (string->dep-sha
           "dependent-sum-template-0.1.0.3@sha256:0bbbacdfbd3abf2a15aaf0cf2c27e5bdd159b519441fec39e1e6f2f54424adde,1682")
          (string->dep-sha
           "hashing-0.1.0.1@sha256:98861f16791946cdf28e3c7a6ee9ac8b72d546d6e33c569c7087ef18253294e7,2816")
          (string->dep-sha
           "monoidal-containers-0.6.0.1@sha256:7d776942659eb4d70d8b8da5d734396374a6eda8b4622df9e61e26b24e9c8e40,2501"))))

;; --------------------------------------
;; LLVM Extra Dependency groups
;; --------------------------------------

(defparameter *llvm-hs-deps*
  (make-groups :comment "LLVM-HS Library dependencies"
               :deps (list
                      (make-dependency-bare :name "llvm-hs-pure-9.0.0")
                      (make-dependency-bare :name "llvm-hs-9.0.1")
                      (make-dependency-bare :name "llvm-hs-pretty-0.9.0.0"))))

;; --------------------------------------
;; Interaction Net Groups Dependencies
;; --------------------------------------

(defparameter *interaction-net-extra-deps*
  (make-groups :comment "For Interaction Nets json-schema"
               :deps (list
                      (make-dependency-github
                       :name "cryptiumlabs/jsonschema-gen"
                       :commit "0639cd166ec59a04d07a3a7d49bdf343e567000e"))))

;; --------------------------------------
;; Servant extra dependency groups
;; --------------------------------------

(defparameter *servant-deps*
  (make-groups :comment "For HTTP server"
               :deps (list (make-dependency-bare :name "servant-static-th-1.0.0.0"))))


;; --------------------------------------
;; General Extra Groups
;; --------------------------------------

(defparameter *eac-solver*
  (make-groups :comment "For the EAC Solver"
               :deps (list
                      (make-dependency-github
                       :name "cwgoes/haskell-z3"
                       :commit "889597234bcdf5620c5a69d3405ab4d607ba4d71"))))

(defparameter *tasty-silver*
  (make-groups :comment "Testing with tasty silver"
               :deps (list
                      (make-dependency-github
                       :name "phile314/tasty-silver"
                       :commit "f1f90ac3113cd445e2a7ade43ebb29f0db38ab9b")
                      *tasty*)))


(defparameter *withdraw*
  (make-groups :comment "Witherable"
               :deps (list
                      (string->dep-sha
                       "witherable-0.3.5@sha256:6590a15735b50ac14dcc138d4265ff1585d5f3e9d3047d5ebc5abf4cd5f50084,1476")
                      (string->dep-sha
                       "witherable-class-0@sha256:91f05518f9f4af5b02424f13ee7dcdab5d6618e01346aa2f388a72ff93e2e501,775"))))

(defparameter *graph-visualizer*
  (make-groups
   :comment "Visualizing graphs"
   :deps (list
          (string->dep-sha "fgl-visualize-0.1.0.1@sha256:e682066053a6e75478a08fd6822dd0143a3b8ea23244bdb01dd389a266447c5e,995"))))

;; -----------------------------------
;; stack-yaml for the YAML helpers
;; -----------------------------------

;; TODO ∷ deprecate this when we have dependencies imply other
;; dependencies we should bring in
(defparameter *standard-library-extra-deps*
  (merge-group
   (make-groups
    :comment "Standard Library Extra Dependency"
    :deps nil)
   *tasty-silver*)
  "Extra dependencies for the standard library")


(defun make-general-dependencies (&rest deps)
  (make-groups :comment "General Dependencies" :deps deps))

(defun big-dep-list ()
  "For the packages with lots of dependecies, these tend to be the
common ones to include"
  (list (make-general-dependencies *capability*
                                   *prettiest*
                                   *extensible*
                                   *aeson-options*
                                   *un-exceptionalio*
                                   *sr-extra*)
        *withdraw*
        *graph-visualizer*
        *standard-library-extra-deps*
        *morley-sub-deps*
        *morley-sub-deps-extra*
        ;; Context Dependencies
        *stm-container-group*
        *interaction-net-extra-deps*
        *morley-arithmetic-circuit-deps*
        *sub-morley-arithmetic-circuit-deps*))

;;; ----------------------------------------------------------------------
;;; stack-yaml for the YAML generation
;;; ----------------------------------------------------------------------

(defun general-dependencies (&rest more-deps)
  (apply #'make-general-dependencies
         (append (list *capability*
                       *prettiest*
                       *galois-field*
                       *bit-vec*
                       *elliptic-curve*)
                 more-deps)))

(defun nix-enable-with (&rest packages)
  (make-nix :enabled t
            :packages packages
            :shell-options (list "--keep" "NIX_SSL_CERT_FILE" "--keep" "STACK_ROOT")
            :pure t))

(defun nix-disable ()
  (make-nix :enabled nil
            :pure t))

(defun nix-enable-custom ()
  (make-nix :enabled t
            :shell-options (list "--keep" "NIX_SSL_CERT_FILE" "--keep" "STACK_ROOT")
            :shell-file "stack.nix"
            :pure t))

(defun nix-enable-zlib (&rest packages)
  (apply #'nix-enable-with *nix-zlib* packages))

(defun nix-enable-llvm (&rest packages)
  (apply #'nix-enable-with *nix-llvm-9* packages))

(defun nix-enable-all (&rest packages)
  (apply #'nix-enable-with *nix-llvm-9* *nix-zlib* *nix-curl* *nix-time* *nix-ldb* packages))

(defparameter *standard-library*
  (make-stack-yaml
   :name "StandardLibrary"
   :nix-build  (nix-enable-custom)
   :extra-deps (list (general-dependencies) *standard-library-extra-deps*)))

(defparameter *sexp*
  (make-stack-yaml
   :name "Sexp"
   :nix-build  (nix-enable-custom)
   :packages   (list *standard-library*)
   :extra-deps (list (general-dependencies)
                     *standard-library-extra-deps*)))

(defparameter *parsing*
  (make-stack-yaml
   :name       "Parsing"
   :nix-build  (nix-enable-custom)
   :packages   (list *standard-library*)
   :extra-deps (list (general-dependencies) *standard-library-extra-deps*)))

(defparameter *context*
  (make-stack-yaml
   :name     "Context"
   :nix-build  (nix-enable-custom)
   :packages   (list *standard-library* *sexp*)
   :extra-deps (list (general-dependencies)
                     *standard-library-extra-deps*
                     *stm-container-group*)))

(defparameter *berlin-pipeline*
  (make-stack-yaml
   :name "BerlinPipeline"
   :nix-build  (nix-enable-custom) ;; (nix-enable-zlib)
   :packages   (list *standard-library* *context* *sexp*)
   :extra-deps (big-dep-list)
   :extra      "allow-newer: true"))

(defparameter *data-structures*
  (make-stack-yaml
   :name     "Test/DataStructures"
   :path-to-other "../../"
   :nix-build  (nix-enable-custom)
   :packages   (list *standard-library* *sexp* *context*)
   :extra-deps (list (general-dependencies)
                     *standard-library-extra-deps*
                     ;; Context Dependencies
                     *stm-container-group*)))

(defparameter *core*
  (make-stack-yaml
   :name       "Core"
   :nix-build  (nix-enable-custom)
   :packages   (list *standard-library* *sexp*)
   :extra-deps (list (general-dependencies *extensible*)
                      *standard-library-extra-deps*
                      *eac-solver*)))

(defparameter *translate*
  (make-stack-yaml
   :name "Translate"
   :nix-build  (nix-enable-custom)
   :packages   (list *core*
                     *parsing*
                     *standard-library*
                     *sexp*
                     *context*
                     *data-structures*
                     *berlin-pipeline*)
   :extra-deps (list (general-dependencies *extensible*)
                     *standard-library-extra-deps*
                     *eac-solver*
                     ;; Context Dependencies
                     *stm-container-group*)))



;; Define these before pipeline due to mutual recursion
(defparameter *pipeline*
  (make-stack-yaml
   :packages (list *standard-library*
                   *sexp*
                   *parsing*
                   *core*
                   *translate*
                   *context*
                   *berlin-pipeline*
                   *data-structures*)
   ;; hack name, for sub dirs
   :name "Pipeline"
   :nix-build  (nix-enable-custom) ;; (nix-enable-zlib *nix-curl*)
   :extra-deps (big-dep-list)
   :extra "allow-newer: true"))

(defparameter *llvm*
  (make-stack-yaml
   :name "Backends/LLVM"
   :path-to-other "../../"
   :packages (list *standard-library*
                   *berlin-pipeline*
                   *core*
                   *context*
                   *pipeline*
                   *parsing*
                   *sexp*
                   *translate*
                   *data-structures*)
   :extra-deps (list (make-general-dependencies *capability* *extensible* *prettiest*)
                     *llvm-hs-deps*

                     ;; for pipeline
                     *graph-visualizer*
                     *morley-sub-deps*
                     *morley-sub-deps-extra*
                     *morley-arithmetic-circuit-deps*

                     ;; Context Dependencies
                     *stm-container-group*

                     ;; for standard-library
                     *standard-library-extra-deps*)
   :nix-build (nix-enable-custom) ;; (nix-enable-llvm *nix-zlib*)
   :extra "allow-newer: true"))

(defparameter *michelson*
  (make-stack-yaml
   ;; hack name, for sub dirs
   :name "Backends/Michelson"
   :path-to-other "../../"
   :nix-build     (nix-enable-zlib *nix-curl*)
   :packages      (list *standard-library* *core* *pipeline* *context*
                        ;; this is needed due to pipeline additions
                        ;; have left it unable to build. I think due to cyclic dependencies
                        *parsing*
                        *sexp*
                        *data-structures*
                        *translate*)
   :extra-deps    (list (make-general-dependencies *capability* *extensible* *prettiest*)
                        *withdraw*
                        *eac-solver*
                        *morley-arithmetic-circuit-deps*
                        *morley-deps*
                        *morley-sub-deps*
                        *morley-sub-deps-extra*
                        ;; Context Dependencies
                        *stm-container-group*
                        *graph-visualizer*
                        *standard-library-extra-deps*)
   :extra "allow-newer: true"))

(defparameter *easy*
  (make-stack-yaml
   :path-to-other "../../"
   :nix-build  (nix-enable-custom) ;; (nix-enable-all)
   :packages (list *standard-library*
                   *parsing*
                   *core*
                   *translate*
                   *berlin-pipeline*
                   *michelson*
                   *llvm*
                   *pipeline*
                   *context*
                   *sexp*
                   *data-structures*)
   ;; hack name, for sub dirs
   :name "Playground/Easy"
   :extra-deps (append (big-dep-list) (list *llvm-hs-deps*))
   :extra "allow-newer: true"))

(defparameter *http*
  (make-stack-yaml
   ;; hack name, for sub dirs
   :name "Playground/HTTP"
   :path-to-other "../../"
   :nix-build (nix-enable-custom) ;; (nix-enable-all)
   :packages (list *standard-library*
                   *parsing*
                   *core*
                   *translate*
                   *pipeline*
                   *michelson*
                   *context*
                   *llvm*
                   *sexp*
                   *data-structures*)
   ;; hack name, for sub dirs
   :name "Playground/HTTP"
   :extra-deps (append (list *servant-deps* *llvm-hs-deps*) (big-dep-list))
   :extra "allow-newer: true"))

(defparameter *witch*
  (make-stack-yaml
   :name "Witch"
   :packages   (list *parsing*
                     *standard-library*
                     *core*
                     *translate*
                     *context*
                     *sexp*
                     *pipeline*
                     *data-structures*
                     *michelson*)
   :nix-build  (nix-enable-custom) ;; (nix-enable-zlib)
   :extra-deps (big-dep-list)
   :extra "allow-newer: true"))

(defparameter *project*
  (make-stack-yaml
   :name "Project"
   :packages (list *standard-library*
                   *parsing*
                   *core*
                   *pipeline*
                   *berlin-pipeline*
                   *translate*
                   *michelson*
                   *easy*
                   *http*
                   *llvm*
                   *witch*
                   *context*
                   *sexp*
                   *data-structures*)
   :nix-build (nix-enable-custom)
   :extra-deps (cons *servant-deps* (cons *llvm-hs-deps* (big-dep-list)))
   :path-to-other "./library/"
   :extra "allow-newer: true"))

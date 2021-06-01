(defpackage #:scripts.slex
  (:documentation "provides random choice of races and classes for slex")
  (:use #:cl #:scripts.slex.entity)
  (:local-nicknames (:race-role :scripts.slex.race-role-map))
  (:export
   :random-element
   :pick-all
   :pick-core
   :pick
   :*races* :*roles* :*good-races* :*decent-races* :*gender* :*alignment*))

(in-package :scripts.slex)

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Race and constant list
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



(defrace yuki-onna :syn '(bare-handed cold))

;; good format call
;; (format t "~{~21a~^ ~21a~^ ~21a~^ ~21a~^ ~21a~^~%~}" (map 'list #'identity *roles*))

(defparameter *good-races*
  '#(angel devil illithid spirit))

(defparameter *decent-races*
  (concatenate 'vector
               *good-races*
               '#(
                 elf moon-elf myrkalfr troll dunadan loli orc dwarf egymid deep-elf breton
                 etheraloid veela vikingd yuki-onna)))

(defparameter *gender* '#(male female))
(defparameter *alignment* '#(Chaotic Neutral Lawful))


(defparameter *roles*
  '#(
    ACID-MAGE             ACTIVISTOR            AKLYST                ALTMER                AMAZON
    ANACHRONIST           ANACHRONOUNBINDER     ARCHEOLOGIST          ARTIST                ASSASSIN
    ASTRONAUT             AUGURER               BARBARIAN             BARD                  BINDER
    BLEEDER               BLOODSEEKER           BOSMER                BULLY                 BUTT-LOVER
    CAMPERSTRIKER         CARTOMANCER           CAVEMAN/CAVEWOMAN     CELLAR-CHILD          CHAOS-SORCEROR
    CHEVALIER             COMBATANT             CONVICT               COOK                  COURIER
    CRACKER               CRUEL-ABUSER          CYBERNINJA            DANCER                DEATH-EATER
    DEMAGOGUE             DIABLIST              DISSIDENT             DIVER                 DOLL-MISTRESS
    DOOM-MARINE           DQ-SLIME              DRAGONMASTER          DRUID                 DRUNK
    DUNMER                ELECTRIC-MAGE         ELEMENTALIST          ELPH                  EMERA
    EMPATH                ERDRICK               FAILED-EXISTENCE      FEAT-MASTER           FEMINIST
    FENCER                FIGHTER               FIREFIGHTER           FJORDE                FLAME-MAGE
    FORM-CHANGER          FOXHOUND-AGENT        GAMER                 GANG-SCHOLAR          GANGSTER
    GEEK                  GENDERSTARIST         GLADIATOR             GOFF                  GOLDMINER
    GRADUATE              GRENADONIN            GUNNER                HALF-BAKED            HEALER
    HUSSY                 ICE-MAGE              INTEL-SCRIBE          JANITOR               JEDI
    JESTER                JUSTICE-KEEPER        KNIGHT                KORSAIR               KURWA
    LADIESMAN             LIBRARIAN             LOCKSMITH             LUNATIC               MAHOU-SHOUJO
    MASON                 MASTERMIND            MEDIUM                MIDGET                MILL-SWALLOWER
    MONK                  MURDERER              MUSICIAN              MYSTIC                NECROMANCER
    NINJA                 NOOB-MODE-BARB        NOBLEMAN/NOBLEWOMAN   NUCLEAR-PHYSICIST     OCCULT-MASTER
    OFFICER               ORDINATOR             OTAKU                 PALADIN               PICKPOCKET
    PIRATE                POISON-MAGE           POKEMON               POLITICIAN            PRACTICANT
    PREVERSIONER          PRIEST/PRIESTESS      PROSTITUTE            PSION                 PSYKER
    QUARTERBACK           RANGER                RINGSEEKER            ROCKER                ROGUE
    SAGE                  SAIYAN                SAMURAI               SCIENTIST             SECRET-ADVICE-MEMBER
    SHAPESHIFTER          SHOE-FETISHIST        SLAVE-MASTER          SOCIAL-JUSTICE-WARRIOR SOFTWARE-ENGINEER
    SPACE-MARINE          SPACEWARS-FIGHTER     STORMBOY              SUPERMARKET-CASHIER   SYMBIANT
    THALMOR               TOPMODEL              TOSSER                TOURIST               TRACER
    TRANSSYLVANIAN        TRANSVESTITE          TWELPH                UNBELIEVER            UNDEAD-SLAYER
    UNDERTAKER            USER-OF-STAND         VALKYRIE              WALSCHOLAR            WANDKEEPER
    WARRIOR               WEIRDBOY              WILD-TALENT           WIZARD                XELNAGA
    YAUTJA                YEOMAN                YSEXYMATE             ZOOKEEPER             ZYBORG))

(defparameter *races*
  '#(
    ADDICT                AGGRAVATOR            AK-THIEF-IS-DEAD!     ALBAE                 ALCHEMIST
    ALIEN                 AMERICAN              AMNESIAC              ANCIENT               ANCIPITAL
    ANDROID               ANGBANDER             ANGEL                 AQUARIAN              ARGONIAN
    ASGARDIAN             ASURA                 ATLANTEAN             AZTPOK                BACTERIA
    BASTARD               BATMAN                BEACHER               BLAIT                 BODYMORPHER
    BORG                  BOSSRUSHER            BOVER                 BRETON                BURNINATOR
    CARTHAGE              CELTIC                CENTAUR               CERBERUS              CHANGELING
    CHIQUAI               CHIROPTERAN           CLOCKWORK-AUTOMATON   COCKATRICE            COLORATOR
    CORTEX                CUPID                 CURSER                DARK                  SEDUCER
    SWIKNI                DEATHMOLD             DEEP-ELF              DESTABILIZER          DEVELOPER
    DEVIL                 DINOSAUR              DOLGSMAN              DOPPELGANGER          DORIAN
    DRAGON                DROW                  DRYAD                 DUFFLEPUD             DUNADAN
    DUTHOL                DWARF                 DYNAMO                EGYMID                ELEMENTAL
    ELF                   ENGCHIP               ENT                   EROSATOR              ETHERALOID
    EVILVARIANT           EXPERT                EXTRAVATOR            FAIRY                 FAWN
    FELID                 FEMINIZER             FENEK                 FIEND                 FIXER
    FRENDIAN              FRO                   GASTLY                GAVIL                 GELATINOUS-CUBE
    GERTEUT               GIANT                 GLORKUM               GNOME                 GOAULD
    GOLDEN-SAINT          GOLEM                 GREEN-SLIME           GREMLIN               GREURO
    GRID                  BUG                   GROUPER               HALLUCINATOR          HAXOR
    HC-ALIEN              HEMI-DOPPELGANGER     HEMOPHAGE             HERALD                HERBALIST
    HERETIC               HOBBIT                HOMICIDER             HOUND                 HIDDEN-ELF
    HUMAN                 HYBRIDRAGON           HYPOTHERMIC           ILLITHID              IMMUNIZER
    IMP                   IMPERIAL              INCANTIFIER           INDRAENIAN            INHERITOR
    INKA                  INSECTOID             IRAHA                 IRONMAN               IRRITATOR
    ITAQUE                JABBERWOCK            JAPURA                JAVA                  JELLY
    KHAJIIT               KLACKON               KOBOLD                KOP                   KORONST
    KUTAR                 LEPRECHAUN            LEVELSCALER           LEVITATOR             LICH
    LISTENER              LIZARDMAN             LOLI                  LYCANTHROPE           MACTHEIST
    MAGYAR                MAIA                  MATRAYSER             MAYMES                MAZEWALKER
    METAL                 MIMIC                 MINIMALIST            MISSINGNO             MONGUNG
    MONKEY                MONSTER               MOON-ELF              MOULD                 MUMMY
    MUSHROOM              MYRKALFR              NAGA                  NASTINATOR            NAVI
    NEMESIS               NIBELUNG              NORD                  NULL                  NYMPH
    OCTOPODE              OGRE                  ORC                   OUTSIDER              PEACEMAKER
    PERVERT               PHANTOM               PIECE                 PIERCER               POISONER
    POLYINITOR            PROBLEMATIC           QUANTUM-MECHANIC      RACE-THAT-DOESNT-EXIST RACE-X
    RANDOMIZER            REDDITOR              REDGUARD              RETICULAN             RODNEYAN
    ROHIRRIM              ROOMMATE              ROUGELIKE             RUSMOT                SALAMANDER
    SATRE                 SCRIPTOR              SCURRIER              SEA                   ELF
    SEGFAULTER            SENSER                SERB                  SHEEP                 SHELL
    SHOE                  SINNER                SKELETON              SKILLOR               SNAIL
    SNAKEMAN              SOKOSOLVER            SOVIET                SPAMMER               SPARD
    SPECIALIST            SPIDERMAN             SPIRIT                SPRIGGAN              STAIRSEEKER
    STICKER               SUSTAINER             SUXXOR                SYLPH                 TECHLESS
    THUNDERLORD           TONBERRY              TRAINER               TRANSFORMER           TRAPPER
    TROLL                 TUMBLRER              TURMENE               TURTLE                UMBER-HULK
    UNALIGNMENT-THING     UNBALANCOR            UNDEFINED             UNGENOMOLD            UNICORN
    UNMAGIC               URIAN                 VAMGOYLE              VAMPIRE               VEELA
    VENTURE-CAPITALIST    VIETIS                VIKING                VORTEX                WARPER
    WIND-INHABITANT       WISP                  WOOKIE                WORM-THAT-WALKS       WRAITH
    XORN                  YEEK                  YOKUDA                YUGGER                YUKI-ONNA
    ZRUTY))

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Main functions
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


(defun random-element (xs)
  (elt xs (random (length xs))))


(defun pick-all (&optional (races *races*) (roles *roles*))
  (format t "you are a ~a ~a ~a ~:@a"
          (random-element *alignment*)
          (random-element *gender*)
          (random-element races)
          (random-element roles)))

(defun pick-core (&optional (races *races*) (roles *roles*))
  (format t "you are a ~a ~a"
          (random-element races)
          (random-element roles)))

(defun pick ()
  (labels ((pass-all (f races)
             (let ((race      (random-element races))
                   (role      (random-element *roles*))
                   (gender    (random-element *gender*))
                   (alignment (random-element *alignment*)))
               (funcall f race role gender alignment races)))

           (pick-race (curr-races &optional (print t))
             (when print
               (format t "g - good races~%a - all races~%d - decent races~%"))
             (case (read-char)
               (#\g       *good-races*)
               (#\a       *races*)
               (#\d       *decent-races*)
               (#\Newline (pick-race curr-races nil))
               (t         curr-races)))

           (roll (race role gender alignment curr-races)
             (case (read-char)
               (#\1 (pass-all #'print-info curr-races))
               (#\2 (print-info (random-element curr-races) role gender alignment curr-races))
               (#\3 (print-info race (random-element *roles*) gender alignment curr-races))
               (#\4 (print-info race role gender (random-element *alignment*) curr-races))
               (#\5 (print-info race role (random-element *gender*) alignment curr-races))
               (#\Newline (roll race role gender alignment curr-races))
               (#\c
                (let ((new-races (pick-race curr-races)))
                  (print-info (random-element new-races) role gender alignment new-races)))
               (t nil)))

           (print-info (race role gender alignment curr-races)
             (format t "you are a ~a ~a ~a ~a~%"
                     alignment gender race role)
             (format t "1 - reroll all~%2 - reroll race~%3 - reroll role~%")
             (format t "4 - reroll alignment~%5 - reroll gender~%")
             (format t "c - change races to pick from~%q - quit~%")
             (roll race role gender alignment curr-races)))
    (pass-all #'print-info *races*)))

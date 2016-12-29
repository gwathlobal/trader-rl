(in-package :trader-rl)

(defparameter *settlement-human-names* (list "Nordom" "Yarwick" "Arx" "Urist" "Oklam" "Haswell" "Pascuss" "Hobwall" "Faehnur"  "Bolgoth" "Oockwich" "Hwath" "Quess" "Dwemerrigan"))

(defparameter *settlement-saurian-names* (list "Assurtes" "Assurnibi" "Assardal" "Dussarin" "Issammus" "Kausstari" "Ssapilamat" "Ssegipal" "Assalmika" "Assurtat" "Asserniran" 
                                               "Assurnabit" "Assalmawia" "Addassammu" "Assurammi" "Melkassissi" "Assakussalit"
                                               ))

(defparameter *settlement-tachidi-names* (list "Ix-quatl" "Izz-kraa" "Azz-kitl" "Ax-qui" "Azz-krikl" "Iz-klak" "Ix-tlakl" "Az-zakl" "Azz-lakt" "Zz-krekl" "Ax-kekl" "Az-ketl" 
                                               "Ix-krii" "Iz-quetl" "Ax-lekt"
                                               ))

(defparameter *settlement-gremlin-names* (list "Med Bridrod" "Zrurd Khuggidh" "Irzug" "Bhodkugh" "Azz-krikl" "Laccal" "Grorgukh" "Nilzulbrid" "Vrulbrebard" "Bhargrad Krul" 
                                               "Dharkird Khar" "Lur Udakh" "Bhurd Gadkun" "Khubrogh" "Balgugh"
                                               ))

(defparameter *ruler-human-male-names* (list "Fedar" "Hlenil" "Andramo" "Baladas" "Ano" "Galos" "Balen" "Dunel" "Maner" "Rilos" "Selvil" "Teris" "Ulvon" "Varon"))

(defparameter *ruler-human-female-names* (list "Adrusi" "Badama" "Danaronea" "Ervona" "Felsa" "Garyne" "Haleneri" "Idula" "Llandreri" "Methulu" "Nelso" "Orvona" "Ranis" "Sedris"))

(defparameter *ruler-gremlin-male-names* (list "Zrigs" "Rot" "Kloct" "Xialk" "Klugz" "Lentecs" "Wunaarm" "Olelx" "Larloct" "Kradhuk" "Blekz" "Slel" "Trict" "Slots"))

(defparameter *ruler-gremlin-female-names* (list "Wuinkia" "Stafs" "Frils" "Ofea" "Flif" "Qahbilt" "Criggerti" "Claaglurtia" "Swehdert" "Kheatus" "Karx" "Uhxa" "Niz" "Bhaal"))

(defparameter *ruler-saurian-first-names* (list "Arth" "Zeph" "Ashat" "Deph" "Ars" "Thim" "Aes" "Dath" "Ras" "Cast"))

(defparameter *ruler-saurian-last-names* (list "Nis" "Thoss" "Selis" "Siath" "Lenis" "Neus" "Xeth" "Xiphel"))

(defparameter *ruler-tachidi-first-names* (list "Ix" "Ax" "Axx" "Iz" "Izz"))

(defparameter *ruler-tachidi-second-names* (list "Krat" "Krit" "Quek" "Kret"))

(defparameter *ruler-tachidi-third-names* (list "Xik" "Xak" "Xek" "Tkal" "Tlak" "Quatl"))

(defparameter *ruler-title-names* (list "Wise"
                                        "Wrathful"
                                        "Brave"
                                        "Illustrious"
                                        "Rich"
                                        "First"
                                        "Second"
                                        "Third"
                                        "Architect"
                                        "Benevolent"
                                        "Noble"
                                        "Dreadful"))

(defparameter *race-names* (list "Human"
                                 "Tachidi"
                                 "Gremlin"
                                 "Saurian"))

(defconstant +race-type-human+ 0)
(defconstant +race-type-tachidi+ 1)
(defconstant +race-type-gremlin+ 2)
(defconstant +race-type-saurian+ 3)

(defconstant +settlement-size-village+ 0)
(defconstant +settlement-size-town+ 1)
(defconstant +settlement-size-city+ 2)

(defparameter *settlement-size-names* (list "Village"
                                            "Town"
                                            "City"))

(defconstant +settlement-type-agriculture+ 0)
(defconstant +settlement-type-mining+ 1)
(defconstant +settlement-type-industry+ 2)
(defconstant +settlement-type-sprawling+ 3)

(defparameter *settlement-type-names* (list "agricultural"
                                            "mining"
                                            "industrial"
                                            "sprawling"))

(defconstant +item-type-food+ 0)
(defconstant +item-type-ore+ 1)
(defconstant +item-type-tools+ 2)
(defconstant +item-type-wood+ 3)
(defconstant +item-type-furniture+ 4)
(defconstant +item-type-weapons+ 5)
(defconstant +item-type-crafts+ 6)
(defconstant +item-type-gems+ 7)
(defconstant +item-type-jewelry+ 8)

(defconstant +ruler-audience-tax+ 200)

(defconstant +quest-item-money+ -1)

(defconstant +quest-type-money-donation+ 0)
(defconstant +quest-type-weapons-donation+ 1)
(defconstant +quest-type-gems-donation+ 2)
(defconstant +quest-type-jewelry-donation+ 3)
(defconstant +quest-type-food-donation+ 4)

(defconstant +ruler-favor-win+ 10)

(defconstant +feature-type-palace+ 0)

(defconstant +event-type-famine+ 0)

(defparameter *item-types* (make-hash-table))
(defparameter *settlements* (make-hash-table))
(defparameter *traders* (make-hash-table))
(defparameter *links* (make-hash-table))
(defparameter *realms* (make-hash-table))
(defparameter *events* (make-hash-table))
(defparameter *quest-types* (make-hash-table))
(defparameter *feature-types* (make-hash-table))
(defparameter *event-types* (make-hash-table))

(defparameter *player* nil)
(defparameter *world* nil)

(defparameter *current-dir* (asdf:system-source-directory :trader-rl))
(defparameter *save-on-quit* t) ;; can also take :delete

(in-package :trader-rl)

(defparameter *settlement-names* (list "Nordom"
                                       "Yarwick"
                                       "Arx"
                                       "Urist"
                                       "Oklam"
                                       "Haswell"
                                       "Pascuss"
                                       "Hobwall"
                                       "Faehnur"
                                       "Bolgoth"
                                       "Oockwich"
                                       "Hwath"
                                       "Quess"
                                       "Dwemerrigan"))

(defparameter *ruler-male-names* (list "Fedar"
                                      "Hlenil"
                                      "Andramo"
                                      "Baladas"
                                      "Ano"
                                      "Galos"
                                      "Balen"
                                      "Dunel"
                                      "Maner"
                                      "Rilos"
                                      "Selvil"
                                      "Teris"
                                      "Ulvon"
                                      "Varon"))

(defparameter *ruler-female-names* (list "Adrusi"
                                         "Badama"
                                         "Danaronea"
                                         "Ervona"
                                         "Felsa"
                                         "Garyne"
                                         "Haleneri"
                                         "Idula"
                                         "Llandreri"
                                         "Methulu"
                                         "Nelso"
                                         "Orvona"
                                         "Ranis"
                                         "Sedris"))

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

(defparameter *item-types* (make-hash-table))
(defparameter *settlements* (make-hash-table))
(defparameter *traders* (make-hash-table))
(defparameter *links* (make-hash-table))
(defparameter *palaces* (make-hash-table))
(defparameter *quest-types* (make-hash-table))
(defparameter *player* nil)
(defparameter *world* nil)

(defparameter *current-dir* (asdf:system-source-directory :trader-rl))
(defparameter *save-on-quit* t) ;; can also take :delete

;; additional keywords for company-keywords backend

(with-eval-after-load 'company ;; custom backend
  (require 'company-keywords)
  (setq company-keywords-alist+-additional
        '((javascript-mode
           "console.log"
           "document" "getElementById" "getElementsByClassName" "getElementsByTagName"
           "addEventListener" "removeEventListener"
           "querySelector" "querySelectorAll"
           "window" "setTimeout" "setInterval"
           )
          (scheme-mode
           "list-tabulate"
           )))

  (defun merge-alists (merge a1 a2)
    (let ((result nil))
      (dolist (l1 a1)
        (let ((l2 (assq (car l1) a2)))
          (push (if l2 (cons (car l1) (funcall merge (cdr l1) (cdr l2))) l1)
                result)))
      (dolist (l2 a2)
        (unless (assq (car l2) result)
          (push l2 result)))
      result))

  (setq company-keywords-alist+
        (merge-alists
         (lambda (l1 l2)
           (sort (remove-duplicates (append l1 l2) :test 'string=)
                 'string<))
         company-keywords-alist
         company-keywords-alist+-additional
         ))

  (defun company-keywords+ (command &optional arg &rest ignored)
    "`company-mode' backend for programming language keywords with additional words."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-keywords+))
      (t           (let ((company-keywords-alist company-keywords-alist+))
                     (company-keywords command arg ignored)))))

  (defun company-keywords-web (command &optional arg &rest ignored)
    "Copy of `company-keywords' except:
1. this detects the language in html dynamically (js, css)
2. this uses `company-keywords-alist+' (for additional js words)"
    (interactive (list 'interactive))
    (let ((lang (or (cdr (assoc (web-mode-language-at-pos)
                                '(("javascript" . javascript-mode)
                                  ("css"        . css-mode))
                                'string=))
                    'web-mode)))
      (cl-case command
        (interactive (company-begin-backend 'company-keywords-web))
        (prefix (and (assq lang company-keywords-alist+)
                     (not (company-in-string-or-comment))
                     (or (company-grab-symbol) 'stop)))
        (candidates
         (let ((completion-ignore-case nil)
               (symbols (cdr (assq lang company-keywords-alist+))))
           (all-completions arg (if (consp symbols)
                                    symbols
                                  (cdr (assq symbols company-keywords-alist+))))))
        (kind 'keyword)
        (sorted t))))
  )

(progn ;; english completion
  (setq english-words
        '(
          "about" "right" "there" "really" "because" "little" "never" "sorry" "where"
          "maybe" "anything" "please" "thank" "people" "still" "again" "thing" "great"
          "before" "better" "night" "first" "believe" "after" "these" "around" "always"
          "listen" "those" "wrong" "through" "guess" "remember" "together" "leave"
          "mother" "place" "understand" "actually" "father" "their" "course" "might"
          "every" "enough" "someone" "family" "whole" "another" "house" "yourself"
          "woman" "hello" "which" "money" "tonight" "happy" "pretty" "already" "three"
          "found" "world" "honey" "myself" "exactly" "probably" "while" "alone" "since"
          "excuse" "start" "today" "ready" "until" "without" "whatever" "friend" "stuff"
          "worry" "second" "truth" "school" "forget" "business" "cause" "chance" "anyone"
          "person" "somebody" "heart" "point" "later" "anyway" "phone" "reason" "bring"
          "tomorrow" "trust" "check" "anymore" "least" "beautiful" "crazy" "party"
          "afraid" "between" "important" "everyone" "sister" "minute" "everybody"
          "couple" "either" "daughter" "under" "break" "close" "doctor" "trouble"
          "though" "different" "hospital" "anybody" "alright" "wedding" "perfect"
          "police" "stand" "story" "dinner" "against" "funny" "husband" "almost" "stupid"
          "answer" "office" "child" "yours" "moment" "sleep" "young" "sonny" "lucky"
          "sometimes" "serious" "behind" "inside" "ahead" "wonderful" "fight" "quite"
          "number" "nobody" "death" "along" "finally" "upset" "children" "front" "clear"
          "drink" "absolutely" "sweet" "alive" "sense" "happen" "special" "blood"
          "meeting" "coffee" "sound" "fault" "water" "women" "welcome" "speak" "think"
          "Christmas" "order" "outside" "worse" "company" "mistake" "handle" "spend"
          "totally" "control" "marriage" "power" "president" "unless" "picture" "hundred"
          "change" "explain" "certainly" "relationship" "choice" "anywhere" "secret"
          "future" "weird" "touch" "crane" "question" "obviously" "straight" "grace"
          "white" "drive" "marry" "light" "frank" "dream" "protect" "twenty" "class"
          "surprise" "forever" "except" "dance" "especially" "situation" "besides"
          "himself" "worth" "expect" "rather" "involve" "swear" "piece" "black" "movie"
          "catch" "country" "perhaps" "personal" "admit" "problem" "murder" "strong"
          "honest" "broke" "tired" "evening" "human" "entire" "suppose" "imagine" "blame"
          "street" "apartment" "court" "terrible" "clean" "learn" "relax" "million"
          "charity" "accident" "prove" "smart" "message" "small" "interest" "table"
          "become" "mouth" "pregnant" "middle" "careful" "shall" "figure" "shoot" "stick"
          "follow" "angry" "instead" "buddy" "write" "early" "angel" "forgive" "lunch"
          "eight" "thousand" "music" "tough" "count" "college" "boyfriend" "proud"
          "agree" "birthday" "seven" "history" "share" "offer" "hurry" "wonder" "simple"
          "decision" "building" "finish" "voice" "herself" "would" "deserve" "dress"
          "interesting" "Jesus" "hotel" "enjoy" "quiet" "short" "mention" "clothe"
          "neither" "respect" "spent" "prison" "attention" "normal" "dollar" "lawyer"
          "apart" "certain" "girlfriend" "floor" "whether" "everything" "present" "earth"
          "private" "cover" "judge" "mommy" "possibly" "worst" "station" "accept"
          "strange" "plane" "yesterday" "quick" "lately" "stuck" "lovely" "security"
          "report" "difference" "store" "single" "doubt" "record" "captain" "crime"
          "window" "return" "guilty" "difficult" "service" "magic" "uncle" "promise"
          "public" "bother" "island" "seriously" "broken" "advice" "somehow" "concern"
          "usually" "summer" "thirty" "letting" "officer" "support" "afternoon"
          "apologise" "nervous" "across" "charge" "patient" "brain" "general" "breakfast"
          "horrible" "awful" "pleasure" "driving" "system" "dying" "chief" "faith"
          "month" "visit" "screw" "letter" "decide" "double" "press" "forward" "smell"
          "spell" "memory" "hungry" "board" "position" "hearing" "kitchen" "force"
          "during" "space" "other" "discuss" "third" "fifty" "reading" "idiot" "suddenly"
          "agent" "bunch" "destroy" "track" "shoes" "scene" "peace" "demon" "consider"
          "drunk" "knock" "department" "sooner" "plenty" "extra" "attack" "ground"
          "whose" "weekend" "matter" "wrote" "opportunity" "impossible" "machine" "waste"
          "pretend" "danger" "proof" "complete" "arrest" "perfectly" "twice" "easier"
          "romantic" "comfortable" "divorce" "begin" "closely" "although" "smile" "laugh"
          "treat" "amber" "otherwise" "excited" "green" "stole" "notice" "excellent"
          "paper" "bottom" "sudden" "church" "bathroom" "glass" "contact" "reach"
          "partner" "computer" "study" "chicken" "error" "build" "Saturday" "birth"
          "grandmother" "heavy" "lesson" "speed" "sugar" "market" "process" "stone"
          "serve" "river" "super" "monkey" "signal" "above" "addition" "among" "amount"
          "angle" "animal" "appear" "apple" "belong" "block" "brown" "cannot" "capital"
          "carry" "centre" "century" "circle" "cloud" "column" "common" "compere"
          "condition" "contain" "continue" "create" "cross" "crowd" "describe" "desert"
          "design" "detail" "dictionary" "direction" "effect" "element" "energy" "engine"
          "England" "English" "example" "express" "famous" "field" "finger" "flower"
          "forest" "fresh" "fruit" "garden" "group" "horse" "include" "increase"
          "indicate" "information" "insect" "instrument" "itself" "language" "large"
          "length" "level" "march" "bacon" "morning" "autumn" "aggravation" "Archaic"
          "pertinent" "defense" "planning" "flight" "honestly" "remind" "witness"
          "hardly" "steal" "princess" "silly" "teach" "trial" "radio" "dirty" "choose"
          "emergency" "credit" "obvious" "loving" "positive" "price" "goodbye" "guard"
          "total" "crying" "trick" "pressure" "dressed" "taste" "south" "something"
          "nurse" "raise" "mister" "whoever" "breaking" "writing" "assume" "asleep"
          "legal" "justice" "bedroom" "shower" "camera" "forty" "bigger" "breath" "pants"
          "freak" "French" "action" "folks" "cream" "focus" "truly" "convince" "client"
          "threw" "allow" "grand" "shirt" "chair" "rough" "government" "harry" "going"
          "empty" "round" "insane" "aware" "meaning" "tight" "subject" "guest" "match"
          "confused" "surgery" "bottle" "beyond" "opinion" "naked" "necessary" "barely"
          "health" "video" "cousin" "twelve" "simply" "often" "fifteen" "spirit" "speech"
          "issue" "final" "result" "research" "nowhere" "escape" "biggest" "restaurant"
          "grateful" "usual" "address" "within" "someplace" "everywhere" "train" "regret"
          "goodness" "heaven" "responsibility" "suspect" "corner" "terrific" "mission"
          "further" "truck" "following" "teeth" "ruined" "split" "airport" "older"
          "project" "desperate" "themselves" "search" "damage" "spoke" "quickly" "scare"
          "beach" "afford" "settle" "passion" "Friday" "natural" "champagne" "connection"
          "happiness" "saving" "personally" "suggest" "ticket" "taught" "loose" "staff"
          "planet" "practice" "bright" "warning" "miracle" "carrying" "flying" "blind"
          "queen" "shopping" "monster" "sight" "vampire" "bride" "account" "clearly"
          "celebrate" "brilliant" "custody" "center" "toast" "student" "however"
          "professional" "reality" "attitude" "advantage" "grandfather" "grandma"
          "someday" "grade" "cheese" "pizza" "powerful" "opening" "sally" "eventually"
          "exciting" "covered" "familiar" "television" "harmony" "colour" "schedule"
          "capable" "master" "practically" "correct" "forgotten" "appointment" "social"
          "nature" "teacher" "threat" "bloody" "lonely" "shame" "local" "jacket" "scary"
          "loser" "invite" "merry" "precious" "criminal" "growing" "victim" "professor"
          "funeral" "couch" "strength" "beauty" "several" "shock" "chocolate" "greatest"
          "miserable" "character" "became" "enemy" "crash" "recognize" "healthy" "boring"
          "engage" "percent" "purpose" "north" "knife" "badly" "curious" "paint" "pardon"
          "built" "closet" "candy" "survive" "operation" "trade" "thanksgiving" "revenge"
          "available" "program" "prefer" "spare" "aside" "statement" "sometime"
          "fantastic" "launch" "affair" "depend" "national" "brave" "storm" "prince"
          "interview" "football" "explanation" "style" "assistant" "stronger" "handsome"
          "unbelievable" "anytime" "nearly" "shake" "wherever" "medicine" "lousy"
          "circumstance" "stage" "license" "nothing" "community" "trash" "awake"
          "friendship" "stomach" "weapon" "mystery" "official" "regular" "valley"
          "contract" "sexual" "basically" "switch" "frankly" "cheap" "lifetime"
          "painting" "clock" "weight" "bullet" "indeed" "changing" "particular" "decent"
          "spring" "avoid" "united" "score" "disappear" "stranger" "exact" "recently"
          "fortune" "strike" "insurance" "fancy" "drove" "career" "shape" "stock"
          "fashion" "freedom" "timing" "guarantee" "chest" "bridge" "source" "theory"
          "original" "juice" "access" "watch" "heading" "selfish" "period" "committed"
          "freeze" "noise" "exist" "science" "player" "ceremony" "uncomfortable"
          "weather" "mostly" "stress" "permission" "possibility" "faster" "borrow"
          "release" "junior" "property" "negative" "fabulous" "vision" "member" "battle"
          "devil" "fellow" "apology" "anger" "honeymoon" "parking" "protection" "manager"
          "Chinese" "campaign" "sensitive" "photo" "chose" "comfort" "worrying" "pocket"
          "bleeding" "shoulder" "ignore" "fourth" "talent" "garage" "travel" "success"
          "training" "crack" "model" "radar" "remain" "meantime" "connected" "chase"
          "cancer" "likely" "brother" "driver" "concentrate" "throat" "intend" "ashame"
          "midnight" "manage" "guilt" "terms" "interrupt" "tongue" "distance"
          "conference" "treatment" "basement" "sentence" "purse" "glasses" "cabin"
          "universe" "towards" "repeat" "mirror" "wound" "reaction" "engagement"
          "therapy" "emotional" "magazine" "society" "stake" "awesome" "genius"
          "extremely" "entirely" "nasty" "expensive" "counting" "kidnapping" "square"
          "cleaning" "shift" "plate" "trapped" "charming" "attractive" "argue" "Sunday"
          "settled" "package" "disease" "staircase" "alarm" "nerve" "incredibly" "stamp"
          "becoming" "terribly" "friendly" "easily" "suffering" "disgusting" "deliver"
          "riding" "federal" "disaster" "claim" "turkey" "spoken" "snake" "introduce"
          "rescue" "confession" "lover" "impression" "fantasy" "reputation" "knowledge"
          "Europe" "suffer" "argument" "homework" "cancel" "director" "prepare" "wheel"
          "crisis" "Monday" "gentleman" "toilet" "agreement" "eleven" "popular" "clinic"
          "exchange" "university" "winter" "holiday" "material" "parent" "article"
          "grant" "orange" "Thursday" "dozen" "bread" "silence" "seventy" "butter"
          "carrot" "alcohol" "headache" "tennis" "Tuesday" "theatre" "ninety" "guide"
          "royal" "ability" "tradition" "towel" "habit" "event" "basket" "balance"
          "exercise" "forth" "eighteen" "perform" "everyday" "village" "journey" "supply"
          "helpful" "useless" "fourteen" "replace" "emotion" "stubborn" "provide"
          "nephew" "victory" "title" "furniture" "blanket" "surface" "niece" "gloves"
          "signature" "compound" "abuse" "accurate" "acquire" "barrier" "battery" "beard"
          "behave" "beside" "bicycle" "biscuit" "bonus" "brush" "bucket" "budget"
          "butcher" "carpet" "centimetre" "cereal" "certificate" "champion" "chemical"
          "cheque" "cherry" "cigarette" "cinema" "citizen" "classroom" "darling" "delay"
          "diary" "eastern" "economic" "email" "employ" "enable" "finance" "formula"
          "frequent" "fridge" "fulfil" "industry" "injury" "institute" "journalist"
          "absence" "kettle" "keyboard" "label" "laboratory" "labour" "ladder" "landlord"
          "leader" "leadership" "leather" "leisure" "logical" "lorry" "library" "mainly"
          "maintain" "maintenance" "major" "manner" "margin" "marvellous" "massive"
          "narrow" "native" "naughty" "nearby" "neighbour" "network" "nicely" "no way"
          "object" "objection" "obligation" "observation" "observe" "obtain" "occasion"
          "occupy" "occur" "ocean" "o’clock" "of course" "operate" "oppose" "palace"
          "panic" "parcel" "parliament" "participate" "partly" "passenger" "patience"
          "pattern" "pause" "payment" "qualify" "quality" "quantity" "railway" "range"
          "rapid" "rarely" "react" "receipt" "receive" "safety" "salad" "salary"
          "sandwich" "satellite" "satisfaction" "sauce" "sausage" "scale" "scheme"
          "scratch" "screen" "script" "season" "secondary" "select" "senior" "sensible"
          "separate" "sequence" "servant" "session" "settlement" "severe" "shadow"
          "tablet" "target" "telly" "technology" "telephone" "temperature" "unable"
          "undertake" "uniform" "unlike" "vacation" "variation" "vegetable" "yellow"
          "wander" "wardrobe" "weakness" "wealth" "website" "western" "whatsoever"
          "whereas" "wooden" "workshop" "kilometre" "largely" "lunchtime" "liquid"
          "limit" "literature" "mushroom" "musical" "muscle" "movement" "mouse"
          "mountain" "northern" "overseas" "ownership" "programme" "pupil" "primarily"
          "presume" "predict" "practise" "praise" "potato" "post office" "politician"
          "policeman" "petrol" "pepper" "pension" "pencil" "reduce" "region" "scientist"
          "sheep" "sheet" "shelf" "shout" "singer" "slice" "smoking" "spoon" "steak"
          "strawberry" "strict" "struggle" "submit" "supermarket" "supper" "symbol"
          "territory" "tomato" "tooth" "tourist" "tower" "traditional" "translate"
          "transport" "trousers" "unlikely" "useful" "visual" "volume" "youth"
          "investigation" "T-shirt" "apricot" "attribute" "beetle" "bracket" "concise"
          "decay" "farther" "lavatory" "litre" "metre" "pigeon" "porridge" "raspberry"
          "appreciate" "responsible" "fought" "pride" "solve" "hopefully" "illegal"
          "punch" "mayor" "recall" "defend" "painful" "cooking" "button" "sixty" "coach"
          "awhile" "confidence" "carter" "image" "refuse" "determined" "progress"
          "military" "artist" "cruel" "traffic" "mental" "poison" "expert" "benefit"
          "secretary" "sneak" "privacy" "twins" "swing" "successful" "scream" "solid"
          "flash" "crush" "ambulance" "wallet" "discovery" "officially" "option"
          "laundry" "former" "assure" "accuse" "plant" "lower" "bored" "silver" "sheriff"
          "suite" "section" "commit" "suicide" "spread" "fishing" "league" "proper"
          "fifth" "solution" "sharp" "giant" "latest" "highly" "audience" "winner"
          "insist" "cheer" "flesh" "district" "routine" "adult" "spike" "awfully" "fever"
          "female" "sweat" "silent" "clever" "request" "prize" "fully" "estate" "union"
          "goodnight" "divorced" "despite" "surely" "confess" "bless" "potential" "chill"
          "blonde" "brains" "agency" "degree" "unusual" "joint" "covering" "newspaper"
          "coast" "grave" "direct" "cheating" "quarter" "mixed" "brand" "policy"
          "competition" "reasonable" "curse" "dessert" "rolling" "alien" "cloth"
          "ancient" "value" "secure" "pilot" "penny" "offense" "conscience" "invitation"
          "hidden" "grief" "smiling" "destiny" "pregnancy" "prisoner" "delivery" "virus"
          "shrink" "influence" "freezing" "concert" "wreck" "chain" "walker" "presence"
          "anxious" "version" "wishes" "bound" "charm" "frame" "opera" "nonsense"
          "fright" "downtown" "actual" "classic" "civil" "education" "torture" "location"
          "effort" "owner" "fairy" "necessarily" "county" "contest" "print" "motel"
          "directly" "underwear" "exhaust" "particularly" "carefully" "trace" "smooth"
          "recovery" "intention" "enter" "standard" "sacrifice" "courage" "attract"
          "remove" "intense" "violence" "attempt" "unfair" "loyal" "approach" "slowly"
          "normally" "actor" "plain" "attic" "threaten" "motion" "collection" "incident"
          "failure" "imagination" "counsellor" "opposite" "highest" "gross" "golden"
          "badge" "Italian" "visiting" "studio" "naturally" "frozen" "trunk" "armed"
          "twisted" "costume" "temporary" "sixteen" "kitty" "honorary" "priest" "cable"
          "affect" "happily" "leading" "authority" "commercial" "admire" "deeply"
          "wrapped" "tense" "sport" "route" "plastic" "election" "pierce" "mortal"
          "fascinating" "chosen" "shown" "abandon" "china" "arrangement" "agenda"
          "series" "literally" "propose" "honesty" "basketball" "underneath" "soldier"
          "review" "lecture" "eighty" "brandy" "relief" "counter" "transfer" "response"
          "channel" "backup" "identity" "differently" "biological" "minor" "creep"
          "waitress" "skill" "reception" "diner" "annoying" "council" "sergeant"
          "international" "blast" "basic" "clown" "customer" "creature" "prime"
          "handling" "eaten" "therapist" "comment" "reporter" "priority" "warehouse"
          "virgin" "loyalty" "inspector" "candle" "pleasant" "media" "castle" "permanent"
          "financial" "demand" "assault" "motive" "unconscious" "museum" "alley"
          "swimming" "mysterious" "unhappy" "liberty" "award" "cooper" "childhood"
          "causless" "sample" "background" "misery" "central" "boots" "thief" "squeeze"
          "lobby" "drama" "noble" "facing" "poker" "cookie" "digging" "creepy"
          "comparable" "trail" "saint" "rotten" "liver" "drawn" "device" "magical"
          "moral" "attached" "searching" "depressed" "aisle" "underground" "proposal"
          "arrange" "annulment" "squad" "represent" "product" "afterward" "adventure"
          "resist" "piano" "darkness" "violent" "strip" "celebration" "below" "paperwork"
          "mighty" "typical" "stable" "pound" "pillow" "mature" "current" "tension"
          "stroke" "steady" "overnight" "meanwhile" "chips" "boxing" "collect" "tragedy"
          "therefore" "spoil" "realm" "profile" "surgeon" "stretch" "confident"
          "perspective" "designer" "climb" "punishment" "finest" "twist" "trigger"
          "proceed" "jersey" "fries" "worries" "handy" "disappoint" "crawl" "convicted"
          "counsel" "thick" "minister" "jewellery" "goose" "average" "creative"
          "compliment" "Wednesday" "burger" "seventeen" "harbour" "strategy" "similar"
          "invent" "announce" "slave" "statue" "surrender" "envelope" "sunshine"
          "potatoes" "application" "random" "guitar" "kidney" "essay" "fiancée"
          "kindness" "elephant" "jeans" "terror" "November" "September" "October"
          "pronounce" "blink" "abroad" "absorb" "academic" "acceptable" "accommodation"
          "accompany" "achieve" "achievement" "adapt" "adequate" "adjust"
          "administration" "adopt" "advance" "advertisement" "advise" "aggressive"
          "agriculture" "backwards" "beforehand" "behalf" "beneath" "bloke" "boiler"
          "border" "bounce" "branch" "breast" "brick" "brief" "briefly" "broad" "buyer"
          "cabinet" "calculate" "calendar" "candidate" "capacity" "capture" "catalogue"
          "category" "cease" "ceiling" "chairman" "championship" "chapter"
          "characteristic" "chart" "cheek" "chemist" "chemistry" "clerk" "click"
          "climate" "collapse" "collar" "colleague" "consent" "daily" "database" "debate"
          "decade" "declare" "decline" "define" "definition" "deliberately" "democracy"
          "demonstration" "dentist" "departure" "dependent" "deposit" "depth" "derive"
          "desire" "detect" "determination" "discipline" "discount" "dismiss" "display"
          "economics" "economy" "edition" "editor" "effective" "efficiency" "elderly"
          "electric" "electricity" "elsewhere" "emerge" "emphasis" "empire" "employer"
          "employee" "encounter" "encourage" "engineer" "enhance" "enormous" "facility"
          "factor" "factory" "feature" "feedback" "feeling" "fence" "festival" "fetch"
          "filthy" "flavour" "flood" "foreign" "formal" "formally" "formation"
          "fortnight" "fortunate" "foundation" "freeway" "freezer" "frequently"
          "function" "fundamental" "gallery" "garlic" "gather" "generally" "generation"
          "gentle" "genuine" "glance" "global" "govern" "gradually" "grammar" "graph"
          "grass" "grocery" "halfway" "handbag" "headquarter" "height" "hence" "hesitate"
          "highlight" "highway" "historian" "historical" "holder" "holding" "household"
          "housing" "heavily" "ice cream" "ideal" "identify" "illness" "illustrate"
          "impact" "implement" "implication" "imply" "import" "impose" "improvement"
          "improve" "income" "independence" "independent" "index" "individual"
          "inevitably" "infant" "infection" "inflation" "initial" "initiative"
          "innovation" "input" "inquiry" "install" "instance" "instant" "instruction"
          "investment" "involvement" "jumper" "justify" "luckily" "lounge" "limitation"
          "lighting" "locate" "motorway" "mortgage" "moreover" "mobile phone" "mixture"
          "numerous" "nuisance" "nuclear" "nowadays" "novel" "notion" "notably" "noisy"
          "newly" "nevertheless" "objective" "occasionally" "occupation" "opponent"
          "opposition" "ordinary" "organ" "organic" "organization" "organize" "origin"
          "originally" "outcome" "output" "outstanding" "overall" "overcome" "overtime"
          "pursue" "purple" "purely" "purchase" "pudding" "publish" "publicity"
          "publication" "psychology" "psychological" "provision" "providing" "protest"
          "prosecution" "proportion" "properly" "prompt" "promotion" "promote" "profit"
          "profession" "producer" "produce" "privilege" "prior" "printer" "principle"
          "principal" "primary" "previously" "previous" "prevent" "presumably" "preserve"
          "presentation" "preference" "precisely" "precise" "prayer" "practical"
          "poverty" "poster" "possession" "possess" "pollution" "politics" "poetry"
          "platform" "pitch" "physics" "physically" "phrase" "photograph" "photocopy"
          "philosophy" "phase" "persuade" "permit" "percentage" "perceive" "peaceful"
          "quote" "racing" "radical" "rapidly" "ratio" "reader" "readily" "realistic"
          "recipe" "reckon" "recommend" "recover" "reduction" "refer" "reflect" "reform"
          "regard" "regime" "regularly" "reject" "relate" "relation" "relative" "relieve"
          "religion" "remark" "reserve" "resort" "resource" "respond" "restore"
          "restrict" "restriction" "retire" "reveal" "revolution" "scientific" "scope"
          "secondly" "seize" "selection" "sharply" "shave" "shell" "shelter" "shine"
          "shortly" "should" "shove" "shrug" "similarly" "skirt" "slide" "slight"
          "slightly" "slope" "spray" "starve" "status" "steel" "storage" "strain" "straw"
          "stream" "strongly" "structure" "substance" "succeed" "sufficient" "surprising"
          "surround" "survey" "survival" "suspicion" "sympathy" "tendency" "trend"
          "tricky" "tunnel" "ultimate" "understanding" "unemployed" "unfortunate"
          "unique" "unknown" "upper" "urban" "urgent" "valuable" "various" "vehicle"
          "virtually" "virtue" "visible" "visitor" "vital" "voluntary" "withdraw"
          "widespread" "widely" "whisper" "youngster" "crocodile" "dialect" "e-book"
          "hearty" "incurable" "jockey" "junk food" "mania" "margarine" "mosque"
          "preposition" "proclaim" "shiver" "skilful" "socket" "sportsman somewhere"
          "sweetheart" "amazing" "definitely" "evidence" "upstairs" "dangerous"
          "innocent" "apparently" "congratulations" "experience" "medical" "incredible"
          "attorney" "jealous" "ourselves" "unfortunately" "whenever" "pathetic"
          "downstairs" "immediately" "gorgeous" "physical" "garbage" "coincidence"
          "generous" "outfit" "suspicious" "anniversary" "commitment" "challenge"
          "discussion" "younger" "romance" "procedure" "medication" "shared" "warrant"
          "specific" "sweater" "diagram" "vulnerable" "tears" "locker" "awkward"
          "delicious" "murderer" "technically" "performance" "touching" "committee"
          "consequence" "testimony" "granted" "political" "equipment" "appropriate"
          "impressive" "intelligence" "questioning" "campus" "hallway" "popcorn" "makeup"
          "madam" "cowboy" "blackmail" "concept" "supportive" "memorial" "Japanese"
          "explosion" "trauma" "Russian" "furious" "cheat" "approve" "drawer" "phony"
          "joining" "interfere" "governor" "catching" "bargain" "tragic" "punish"
          "insult" "absolute" "strictly" "British" "worthy" "serving" "polite" "internal"
          "bitter" "adorable" "alike" "distracted" "escort" "circus" "audition"
          "helpless" "dated" "robbery" "beast" "talented" "struck" "mistaken" "Italy"
          "bizarre" "scaring" "focused" "alert" "activity" "foolish" "attend" "Canadian"
          "wheelchair" "protective" "picnic" "construction" "wives" "stink" "recent"
          "Jewish" "filling" "cruise" "cottage" "corporate" "upside" "German" "basis"
          "wounded" "conformity" "wicked" "merely" "massage" "costs" "betray" "waiter"
          "fraud" "cliff" "expression" "entrance" "drawing" "bunny" "bracelet" "thirteen"
          "scout" "fairly" "arrive" "nanny" "naive" "authorities" "separated" "patch"
          "devastated" "description" "subtle" "garrison" "metal" "executive" "confirm"
          "parade" "booth" "straighten" "honoured" "specifically" "convenient"
          "traveling" "laying" "crystal" "apply" "legend" "kindly" "grandson" "donor"
          "temper" "proven" "monitor" "eternity" "denial" "swell" "episode" "thinking"
          "spirits" "potion" "rehearsal" "hostage" "hammer" "facet" "discover" "constant"
          "bench" "moron" "impress" "entitled" "connect" "needle" "intelligent"
          "disagree" "tiger" "groom" "gesture" "developed" "constantly" "sealed"
          "paradise" "legally" "psychic" "dense" "teenage" "rabbit" "puppy" "Bible"
          "accidentally" "superior" "slack" "homeless" "hollow" "desperately" "critical"
          "coward" "personnel" "majesty" "instinct" "extreme" "belief" "motorcycle"
          "convincing" "appeal" "greater" "upsetting" "production" "invisible"
          "forgiveness" "complex" "compare" "blank" "treasure" "sacred" "inviting"
          "inner" "compromise" "cocktail" "tramp" "signing" "messenger" "landing"
          "intimate" "dignity" "dealt" "entertainment" "dressing" "blessing" "billion"
          "lightning" "corky" "alternative" "seduce" "modern" "liquor" "stuffed"
          "emotionally" "division" "conditions" "transplant" "powder" "oxygen" "lunatic"
          "drill" "complain" "announcement" "pumpkin" "mutual" "hockey" "graduate"
          "yacht" "fried" "extraordinary" "appearance" "sworn" "safely" "reunion" "burst"
          "experiment" "explicit" "commission" "chaos" "aboard" "lesbian" "expose"
          "environment" "spider" "smaller" "booze" "tattoo" "parole" "ditch" "bulldog"
          "Spanish" "thirsty" "skull" "scotch" "dining" "unexpected" "pancake" "harsh"
          "existence" "troubles" "favourite" "driven" "bubble" "undercover" "spoiled"
          "deputy" "conspiracy" "clothing" "thoughtful" "drank" "contrary" "beloved"
          "allergic" "forgiven" "approval" "jungle" "dancer" "cotton" "cooked" "peanut"
          "horror" "stunt" "portrait" "jealousy" "hopeless" "volunteer" "sword"
          "scenario" "necklace" "chapel" "restrain" "homicide" "helicopter" "firing"
          "safeguard" "diamond" "devoted" "auction" "videotape" "appetite" "patrol"
          "ironic" "excitement" "anyhow" "bowling" "belly" "shark" "miller" "dealer"
          "cooperate" "bachelor" "accomplish" "reservation" "ashes" "supposedly"
          "integrity" "qualified" "inappropriate" "immediate" "ginger" "sunset" "nation"
          "lipstick" "compassion" "cafeteria" "scarf" "obsession" "management" "lighten"
          "granddaughter" "explode" "balcony" "conscious" "absurd" "vicious" "forbid"
          "defendant" "salesman" "insanity" "genetic" "fighter" "burden" "swallow"
          "kidnap" "entering" "setup" "serial" "sandy" "dramatic" "carver" "blade"
          "seventh" "semester" "delicate" "oldest" "eager" "doomed" "coffin" "bureau"
          "adoption" "sickness" "floating" "combination" "chamber" "casino" "vault"
          "payback" "healing" "cascade" "wrestling" "sixth" "passionate" "lawsuit"
          "distrustul" "crossing" "associate" "journal" "risky" "honourable" "favour"
          "culture" "closest" "breakdown" "conflict" "actress" "wisdom" "steam"
          "worthless" "injured" "graduation" "disturbing" "disturb" "distract"
          "conclusion" "baker" "muffin" "measure" "crawling" "congress" "briefcase"
          "whistle" "roast" "Greek" "flirting" "damaged" "topic" "minimum" "hostile"
          "embarrass" "casual" "beacon" "amusing" "altar" "skinny" "goods" "porch"
          "ghost" "drops" "dizzy" "breastfeeding" "beaten" "rehabilitation"
          "photographer" "leery" "fortunately" "expectation" "draft" "active" "musician"
          "executed" "examine" "document" "bribe" "prescription" "fuzzy" "fragile"
          "forensics" "expense" "drugged" "conduct" "comic" "avenue" "suitcase" "motor"
          "installation" "insecure" "gamble" "wrist" "nicer" "guardian" "dodge" "thumb"
          "limited" "lighter" "elder" "shooter" "quietly" "erase" "denying" "ankle"
          "amnesia" "hunter" "heartbeat" "confront" "minus" "legitimate" "hurricane"
          "fixing" "arrogant" "slightest" "saying" "paternity" "catholic" "snack"
          "rational" "passport" "advanced" "tumor" "humiliated" "destruction" "banana"
          "August" "aspirin" "academy" "throughout" "logic" "knight" "eyesight" "equal"
          "ritual" "perfume" "hiring" "fusion" "elect" "thankful" "nineteen" "comedy"
          "analysis" "teenager" "shore" "detective" "widow" "tissue" "repay"
          "permanently" "deadly" "comfortless" "verdict" "insensitive" "triple" "messy"
          "entry" "bleed" "foster" "airplane" "worker" "underestimate" "soccer"
          "registered" "multiple" "harmless" "frisk" "convention" "communicate"
          "attraction" "arson" "whack" "residence" "medium" "liking" "development"
          "develop" "dearest" "congratulate" "April" "alliance" "vengeance" "puzzle"
          "guidance" "courtesy" "caller" "wizard" "repair" "curiosity" "barbecue"
          "troops" "cough" "accusation" "resent" "freshman" "drown" "highness" "drain"
          "welfare" "theirs" "state" "somewhat" "finishing" "album" "understandable"
          "gratitude" "faithful" "accent" "wandering" "regardless" "inevitable" "crushed"
          "contingent" "rocky" "retired" "gossip" "gambling" "determine" "cosmetics"
          "stiff" "sincere" "shield" "nightmare" "ignoring" "hunch" "firework" "crown"
          "cooperation" "brass" "sophisticated" "luggage" "lemon" "investigate" "explore"
          "dragon" "creek" "complication" "righteous" "reconsider" "inspiration" "goody"
          "fringe" "ethic" "courthouse" "camping" "assistance" "affection" "lodge"
          "haircut" "forcing" "eternal" "sailor" "operator" "exclusive" "destructive"
          "defeat" "adore" "warrior" "shorts" "ninth" "dough" "creation" "barrel"
          "pacific" "magnificent" "goddess" "glorious" "visitation" "scholarship"
          "kingdom" "flames" "sunny" "mattress" "lobster" "importantly" "glove"
          "disappointment" "condo" "cemetery" "screech" "dedicated" "Christian" "annual"
          "polish" "funds" "defensive" "compete" "balloon" "sailing" "filing"
          "depression" "conversation" "consideration" "consciousness" "innocence"
          "freaky" "forehead" "inform" "delighted" "daylight" "currently" "confidential"
          "washing" "warden" "temple" "mistress" "marrow" "hatred" "grill" "corpse"
          "sober" "larger" "infected" "humanity" "electrical" "distraction" "chopper"
          "broadcast" "violation" "suspend" "sting" "promising" "harassment" "gathering"
          "cursed" "content" "combat" "brutal" "asset" "warlock" "wagon" "unpleasant"
          "robot" "lease" "killer" "flame" "domestic" "divine" "disappearance"
          "depressing" "thrill" "terminal" "flush" "exception" "earring" "deadline"
          "corporal" "update" "smack" "madness" "eagle" "delusional" "could" "burnt"
          "tender" "sperm" "realise" "interrogation" "institution" "esteem"
          "communication" "choir" "plague" "manipulate" "lifestyle" "insulting" "honour"
          "detention" "delightful" "daisy" "coffeehouse" "chess" "betrayal" "apologizing"
          "whipped" "reminder" "faint" "confusion" "nearest" "illusion" "execution"
          "distress" "cutter" "correctly" "complaint" "trophy" "pointless" "pearl"
          "eighth" "alibi" "shiny" "keeper" "hobby" "fitting" "curtain" "counselling"
          "technical" "puppet" "modeling" "irresponsible" "humiliation" "felony" "choke"
          "blackmailing" "tabloid" "rally" "pledge" "nursery" "louder" "investigator"
          "ignate" "homecoming" "graduated" "frustrating" "fabric" "distant" "bustle"
          "sleeve" "irony" "torch" "substitute" "scandal" "prick" "laser" "hysterical"
          "growth" "dimension" "climbing" "ultimately" "roller" "negotiate" "millennium"
          "marsh" "majority" "lethal" "cigar" "babysitter" "sponge" "sleepy" "outrageous"
          "medal" "grudge" "driveway" "deserted" "definite" "nickname" "conviction"
          "weigh" "rocket" "intruder" "cycle" "hardware" "wealthy" "versus" "postpone"
          "celebrity" "offensive" "hairy" "bedtime" "alcoholic" "sticky" "splendid"
          "shrimp" "despise" "atmosphere" "rifle" "handwriting" "shepherd" "importance"
          "fatal" "separation" "method" "diagnosis" "yearbook" "triangle" "partnership"
          "humble" "thorough" "thunder" "located" "grandchildren" "sentimental"
          "marketing" "population" "scent" "fleet" "sketch" "outer" "January" "maximum"
          "speaker" "tease" "peach" "clearance" "anxiety" "salmon" "summit" "mechanic"
          "tobacco" "missile" "steer" "granddad" "tournament" "approximately"
          "employment" "grandparents" "forbidden" "tutor" "modest" "vessel" "variety"
          "smash" "shaft" "rainbow" "grasp" "fireplace" "mustard" "wheat" "fountain"
          "December" "writer" "tribe" "ounce" "convenience" "invest" "difficulty"
          "sadness" "microwave" "additional" "mankind" "laptop" "saddle" "razor"
          "coverage" "corridor" "sidewalk" "notify" "confide" "scrub" "parallel" "orbit"
          "feather" "farmer" "spicy" "napkin" "efficient" "arrow" "abbey" "solitary"
          "industrial" "hostess" "delight" "imaginary" "elegant" "elbow" "custom" "tempt"
          "splash" "shade" "metaphor" "ancestor" "acceptance" "weekly" "warmth"
          "umbrella" "parenting" "onion" "mobile" "footstep" "cement" "bloom" "triumph"
          "greed" "essential" "temptation" "sarcasm" "partial" "inconvenience"
          "courageous" "sorrow" "impulsive" "harvest" "divide" "online" "invasion"
          "consultant" "banking" "stereo" "neutral" "harass" "donkey" "cologne" "waist"
          "stepfather" "lemonade" "vomit" "stadium" "liberal" "spice" "tenth" "horizon"
          "frequency" "flexible" "dairy" "corrupt" "herbal" "generosity" "shampoo"
          "humour" "frost" "volcano" "password" "merit" "conquer" "orientation" "lotion"
          "hazard" "glamorous" "scenery" "globe" "verify" "criticism" "thirst" "unpack"
          "sphere" "preparation" "headline" "grape" "vitamin" "uncertain" "stern"
          "sponsor" "peculiar" "grain" "painter" "notebook" "vanish" "tackle" "fisherman"
          "decorate" "salvation" "nausea" "inherit" "passive" "orchestra" "gourmet"
          "tickle" "heating" "scatter" "slavery" "solar" "sculpture" "sensation" "savage"
          "noodle" "vocabulary" "tractor" "probable" "hangover" "thrust" "daring"
          "paragraph" "deceive" "spectacle" "vacant" "shrine" "dedicate" "coupon" "mercy"
          "lively" "equivalent" "compensation" "canvas" "vivid" "outdoor" "litter"
          "gallon" "firmly" "moustache" "marble" "hesitation" "daytime" "shortage"
          "monthly" "renew" "refund" "deliberate" "zipper" "rainy" "overload" "module"
          "merchant" "leftover" "textbook" "flour" "ruler" "currency" "wrinkle"
          "undoubtedly" "timetable" "publisher" "penalty" "queue" "qualification"
          "recognition" "recommendation" "reference" "reflection" "refrigerator"
          "regional" "register" "registration" "regulation" "relatively" "relevant"
          "religious" "remaining" "remarkable" "remote" "replacement" "reply"
          "representation" "republic" "require" "requirement" "resident" "resign"
          "resistance" "resolution" "resolve" "respectively" "retain" "retirement"
          "revenue" "reverse" "reward" "rhythm" "ridiculous" "rival" "roughly" "rubber"
          "rubbish" "rural" "significance" "significant" "significantly" "so-called"
          "software" "solicitor" "southern" "specialist" "species" "specify" "spelling"
          "spill" "spiritual" "spite" "spokesman" "stall" "stare" "steep"
          "straightforward" "strategic" "strengthen" "string" "subsequent" "subsequently"
          "suggestion" "surprisingly" "sustain" "sweep" "technique" "terrorist" "theme"
          "theoretical" "trailer" "transform" "transition" "transportation" "tremendous"
          "troop" "unemployment" "vague" "abnormal" "absent" "abstract" "abundant"
          "accessible" "acute" "addicted" "aesthetic" "agile" "ambiguous" "ambitious"
          "ample" "anonymous" "artificial" "artistic" "assertive" "astonishing"
          "automatic" "balanced" "backyard" "baggage" "ballet" "barbarian" "barber"
          "barefoot" "bathe" "beginner" "beginning" "beige" "being" "beneficial" "biased"
          "bikini" "billboard" "billiards" "biography" "biology" "bishop" "blackboard"
          "blend" "blossom" "blouse" "blown" "blush" "boundary" "breadth" "breakup"
          "breathe" "breed" "breeze" "bribery" "bronze" "browse" "bruise" "buffet"
          "bumper" "bundle" "bureaucracy" "burglar" "cabbage" "calculation" "calculator"
          "calligraphy" "calorie" "camel" "camouflage" "canal" "cannon" "canyon"
          "caption" "carbon" "cardboard" "careless" "cargo" "carpenter" "carriage"
          "carry on" "carsick" "cartoon" "cashier" "cassette" "casualty" "cattle"
          "caught" "caution" "cautious" "cellar" "Celsius" "censor" "centigrade" "chalk"
          "characterize" "chilly" "chimney" "chore" "circuit" "civilian" "civilization"
          "civilize" "clarify" "clash" "classical" "classify" "classmate" "clause"
          "cleaner" "clockwise" "clinical" "cloudy" "clumsy" "coalition" "coastline"
          "cocoa" "coconut" "coercive" "cogent" "coherence" "coherent" "coincident"
          "colloquial" "colonel" "colonialism" "colonist" "colony" "colourful"
          "columnist" "comma" "command" "commentary" "commerce" "commodity" "companion"
          "compass" "Gemini" "scammer" "debut" "decoy" "decrease" "deduct" "defective"
          "dehydrated" "delegate" "delinquent" "dependable" "descend" "descendent"
          "detest" "diagnose" "diameter" "disadvantage" "educate" "eject" "enforce"
          "enlarge" "enquiry" "entrepreneur" "eraser" "erosion" "erupt" "eruption"
          "ethnic" "evaluate" "exaggerate" "exceed" "excite" "exclaim" "exclude"
          "exhibit" "exhibition" "exile" "expansion" "expedition" "expel" "export"
          "external" "eyebrow" "eyelash" "faraway" "fatality" "fatigue" "favourable"
          "fearful" "February" "fertile" "fibber" "fiscal" "fitness" "famine" "flinch"
          "fluctuate" "fluent" "forecast" "foreigner" "format" "fossil" "fraction"
          "fragrance" "framework" "furnish" "gadget" "gangster" "garment" "garnish"
          "generalization" "generalize" "generic" "geography" "geometry" "giggle" "given"
          "glacier" "gloomy" "glory" "graceful" "gradual" "graphics" "groan" "grocer"
          "habitat" "habitual" "handkerchief" "hardy" "harmful" "hassle" "haste"
          "hereditary" "hierarchy" "hinder" "horizontal" "horoscope" "horseback"
          "housewife" "humidity" "humorous" "hydrogen" "hypnotize" "idiom" "ignorant"
          "illiterate" "illogical" "illustration" "imitate" "immigrant" "immigrate"
          "immune" "impartial" "impolite" "income tax" "incomplete" "indirect" "indoor"
          "informal" "inhabitant" "inhale" "inland" "insert" "inspection" "insure"
          "integration" "interested" "intermediate" "internet" "intersection" "interval"
          "invention" "inward" "irritate" "isolate" "ivory" "kneel" "known" "landscape"
          "latitude" "latter" "legislation" "literary" "locally" "loitering" "longitude"
          "lottery" "lumber" "lying" "lyrics" "machinery" "made-up" "magnitude"
          "manufacture" "manufacturer" "March" "married" "mathematics" "meadow" "means"
          "measurement" "melody" "metric" "metropolitan" "midday" "midterm" "mineral"
          "minimal" "missing" "misunderstand" "moderate" "modify" "moist" "moisture"
          "mould" "molecule" "monument" "monopoly" "mosquito" "nationality" "necessity"
          "neglect" "nonfiction" "numeral" "nutrition" "obedient" "obese" "obituary"
          "oblige" "old-fashioned" "Olympics" "omelette" "oneself" "optional" "orthodox"
          "outdoors" "outgoing" "outline" "outlook" "outspoken" "outward" "overdo"
          "overdone" "overestimate" "overflow" "overtake" "overwhelm" "overwork"
          "pajamas" "panel" "parrot" "part-time" "participant" "participation" "particle"
          "paste" "patent" "pedestrian" "peninsula" "perpendicular" "persist"
          "pessimistic" "pianist" "plantation" "plural" "popularity" "postage" "postman"
          "precede" "precedent" "pronunciation" "prospect" "punctual" "racism"
          "radiation" "reasonably" "recycle" "redundancy" "refreshment" "remedy"
          "removal" "renovate" "respectful" "restless" "resume" "rigid" "rinse" "yield"
          "whale" "widen" "width" "windy" "worship" "vacancy" "variable" "vertical"
          "veteran" "viewpoint" "vinegar" "vocational" "vowel" "voyage" "underline"
          "uneasy" "unite" "unity" "unlucky" "upright" "upward" "utility" "telegraph"
          "tentative" "texture" "thermometer" "thesaurus" "thread" "tights" "timber"
          "tolerate" "toward" "translation" "transparent" "treaty" "tremble" "tropical"
          "tulip" "satisfactory" "saucer" "sauna" "scarlet" "scholar" "scissors" "scold"
          "scramble" "scrape" "seafood" "seaside" "selective" "sensual" "shilling"
          "sideways" "simultaneous" "singular" "skeleton" "skyscraper" "slang" "sieve"
          "slender" "softly" "solemn" "sprinkle" "squirrel" "stationery" "stimulate"
          "comprehend" "comprehensive" "compute" "consist" "construct" "consultation"
          "cucumber" "cuisine" "subscribe" "accelerate" "advertise" "wallpaper"
          "waterfall" "attain" "avail" "boast" "confectionery" "dazzle" "debris"
          "deteriorate" "dexterity" "dining-room" "disclose" "disobedience" "dispel"
          "earphones" "extinguish" "eyeshadow" "heaviness" "hemisphere" "herald" "hoarse"
          "hospitable" "hyphen" "hypocrisy" "ignition" "incite" "inclose" "incorrect"
          "instruct" "integral" "intestine" "invoice" "junction" "lavish" "laziness"
          "lever" "libel" "lyrical" "malice" "mammal" "marmalade" "merge" "miser"
          "mistrust" "monarchy" "narrative" "nostril" "notorious" "nought" "nourish"
          "oblivion" "obscure" "ointment" "omission" "orchard" "owing" "pamper" "parsley"
          "pastime" "patron" "pavement" "peasant" "plausible" "predatory" "predominant"
          "proficient" "proverb" "repel" "reside" "ridicule" "rigorous" "robust"
          "seaweed" "semicolon" "situated" "skier" "smith" "snail" "superb" "surname"
          "yearly" "witch" "charges" "smoke" "according" "lieutenant" "breathing"
          "disappointed" "begging" "yourselves" "baseball" "testify" "mansion"
          "personality" "commander" "surveillance" "penthouse" "sneaking" "constable"
          "feeding" "courtroom" "reverend" "enchantment" "drunken" "bullying" "weasel"
          "supreme" "maker" "haunted" "footage" "bogus" "autograph" "spontaneous"
          "probation" "identical" "cooler" "banner" "streak" "spectacular" "sector"
          "heroin" "having" "fisher" "encouraging" "consult" "bailed" "association"
          "maniac" "impulse" "duchess" "classy" "charging" "rising" "hypocrite"
          "humiliate" "hideous" "captured" "betting" "spreading" "ransom" "intent"
          "gladly" "fling" "eliminate" "disorder" "chancellor" "subway" "republican"
          "paralyzed" "dental" "corporation" "cherish" "briefing" "bluff" "fainted"
          "dried" "allright" "acknowledge" "whiskey" "toxic" "skating" "reliable"
          "quicker" "overturn" "lining" "harassing" "endless" "convict" "butler" "rusty"
          "positively" "mount" "manual" "helmet" "failing" "essence" "bully" "airline"
          "hooligan" "pursuit" "greedy" "spine" "shotgun" "reckless" "railroad"
          "psychiatric" "meaningless" "fiancé" "exposure" "evidently" "contempt" "theft"
          "shipment" "scissor" "proposition" "porter" "matching" "marine" "legacy"
          "hormone" "godfather" "gently" "establish" "worldwide" "sexually" "nicest"
          "intern" "handcuff" "franchise" "errand" "entertaining" "barge" "attending"
          "ambassador" "rating" "float" "embrace" "whining" "turner" "receiver" "olive"
          "cheerleader" "unnecessary" "stunning" "shipping" "quest" "luxury" "loosen"
          "haunt" "gracious" "forgiving" "emperor" "abortion" "physician" "perimeter"
          "passage" "longest" "interference" "grease" "eyewitness" "enthusiasm"
          "strongest" "shaken" "portal" "jewel" "concrete" "bearing" "sabotage" "needy"
          "mentor" "listed" "yummy" "valid" "prank" "performing" "obnoxious" "hereby"
          "strangle" "senate" "fiction" "exotic" "demonic" "coloured" "clearing" "brook"
          "boutique" "terrace" "skiing" "righty" "quack" "preliminary" "petty" "ketchup"
          "assist" "violet" "uptight" "terrifying" "swamp" "secretly" "rejection"
          "mentally" "jurisdiction" "deception" "crucial" "cheesy" "arrival" "supporting"
          "scoop" "ribbon" "Easter" "destined" "constitution" "appreciation" "accomplice"
          "sewer" "scroll" "fugitive" "cranky" "bodyguard" "accountant" "whoop"
          "remotely" "protocol" "nickel" "foreman" "decency" "altogether" "squash"
          "ought" "largest" "enforcement" "encumbrance" "donation" "continued"
          "competitive" "businessman" "antique" "advertising" "toothbrush" "retreat"
          "panties" "hourglass" "equally" "consolation" "babble" "stranded" "payslips"
          "overreact" "freshen" "drake" "disposal" "caffeine" "broom" "unfinished"
          "tasty" "syndrome" "stack" "pinch" "isolated" "flatten" "whereabouts" "waiting"
          "truce" "ruling" "poise" "manipulative" "immature" "delivering" "condom"
          "automatically" "anchor" "addict" "throne" "slick" "raining" "pasta" "detector"
          "coolest" "casting" "batch" "almighty" "spark" "perfection" "jelly" "getaway"
          "cracking" "behold" "verge" "timer" "superman" "specialty" "snoop" "leverage"
          "jeopardize" "janitor" "examination" "compact" "clueless" "arriving" "adding"
          "ungrateful" "unacceptable" "shorter" "serum" "irrational" "galaxy"
          "classified" "beautifully" "approaching" "traitor" "sympathetic" "rental"
          "prostitute" "inventory" "improved" "horny" "developing" "commonplace"
          "banging" "amendment" "traumatic" "sweaty" "overboard" "insight" "haven"
          "fluid" "crappy" "chunk" "chandler" "appliance" "stain" "shack" "pervert"
          "occupied" "handful" "flick" "expertise" "embarrassment" "concussion" "summon"
          "splitting" "sneaky" "sloppy" "settling" "reschedule" "notch" "hooray" "extend"
          "exquisite" "disrespect" "amateur" "voting" "shatter" "ruthless" "refill"
          "payroll" "mourning" "marijuana" "manly" "involving" "entertain" "earthquake"
          "drift" "dreadful" "doorstep" "confirmation" "stressful" "preoccupy"
          "predictable" "madly" "embassy" "confuse" "cappuccino" "bouquet" "bailey"
          "amulet" "addiction" "warming" "villa" "unlock" "satisfy" "relaxing" "fudge"
          "elaborate" "concerning" "blocking" "hunger" "hamburger" "greet" "gravy"
          "dreamt" "collecting" "backpack" "agreeing" "supervisor" "starling" "meter"
          "likewise" "irrelevant" "felon" "fasten" "easiest" "disco" "conceivable"
          "compassionate" "backstage" "agony" "adorn" "tweak" "thieve" "surgical"
          "sunrise" "strangely" "recital" "productive" "meaningful" "marching" "kitten"
          "immunity" "frighten" "dearly" "closure" "ambition" "unstable" "sweetness"
          "stinky" "salvage" "petition" "lowlife" "juicy" "intentionally" "inspire"
          "forgave" "devotion" "despicable" "comfy" "breach" "alternate" "twilight"
          "stove" "lengthen" "farewell" "experimental" "caviar" "carnival" "boost"
          "bliss" "token" "temporarily" "superstition" "supernatural" "recorder"
          "presidential" "motivate" "fraternity" "dryer" "chewing" "brake" "bounty"
          "unbelievably" "survivor" "respectable" "premise" "occasional" "indication"
          "gutter" "flashlight" "bloodless" "beware" "shovel" "privately" "locking"
          "heartless" "comparison" "childish" "cardiac" "brace" "blunt" "admission"
          "vanilla" "utterly" "suspension" "sadly" "reserved" "lowest" "kidding" "hitch"
          "flirt" "extension" "establishment" "delayed" "christening" "casket" "broker"
          "antibiotic" "abduct" "witchcraft" "smelly" "running" "protein" "paramedic"
          "newest" "murmur" "marathon" "intact" "grandpa" "diaper" "deceased" "burka"
          "accounting" "shred" "rethink" "pistol" "leash" "hearted" "flown" "feast"
          "extent" "educated" "disgrace" "deposition" "burial" "bookstore" "trespass"
          "optimistic" "jeopardy" "injection" "hilarious" "distinct" "directed" "curve"
          "challenging" "alter" "wilderness" "vindictive" "venture" "teeny" "stroll"
          "sitting" "rebuild" "ordeal" "intimacy" "inheritance" "exploded" "donate"
          "distracting" "despair" "democratic" "cracker" "ammunition" "thoroughly"
          "sheer" "scarecrow" "refreshing" "prosecute" "misplace" "merchandise"
          "membership" "heroic" "facial" "bummer" "syrup" "shuttle" "resemblance"
          "premature" "honours" "gravity" "contribution" "acquainted" "untie" "salute"
          "priceless" "partying" "moonlight" "lightly" "lifting" "glowing" "generator"
          "flowing" "explosive" "cinnamon" "breakthrough" "ballistic" "assassin"
          "antidote" "analyze" "allowance" "adjourn" "understatement" "touchy"
          "subconscious" "roommate" "pitcher" "offend" "knives" "irresistible"
          "incarnate" "hostility" "funky" "equation" "digital" "centered" "watcher"
          "turtle" "transmission" "sleigh" "sarcastic" "recess" "rebound" "rebel"
          "pirate" "living" "heartache" "fundraiser" "dynamite" "doorman" "documentary"
          "discreet" "considerate" "catering" "author" "vacuum" "urine" "strung" "stitch"
          "sordid" "protector" "portion" "kindergarten" "diving" "discharge"
          "confidentiality" "blizzard" "amongst" "woody" "tactic" "straits" "spooky"
          "spaghetti" "powerless" "playground" "paranoia" "hopeful" "havoc" "evaluation"
          "eavesdropping" "doughnut" "diversion" "behead" "aspen" "anyplace" "accessory"
          "titanic" "stuffing" "speeding" "slime" "royalty" "marital" "magician"
          "journalism" "interior" "hatch" "greeting" "ethical" "equipped" "environmental"
          "credibility" "credential" "consistent" "chopped" "bridal" "bedside"
          "babysitting" "authorized" "assumption" "youngest" "witty" "unforgivable"
          "underworld" "sophomore" "selfless" "secrecy" "runway" "programming"
          "professionally" "moving" "meltdown" "incoming" "funding" "comedian" "buckle"
          "assembly" "admired" "adjustment" "slippery" "ridge" "queer" "graveyard"
          "gifted" "cynical" "assassination" "verbal" "unpredictable" "stoop" "plumbing"
          "lingerie" "layer" "fluffy" "dresser" "chauffeur" "bouncing" "sanity"
          "primitive" "pending" "orderly" "obsessive" "motto" "meteor" "glimpse" "froze"
          "execute" "ensure" "dispute" "consulate" "amend" "syringe" "symphony" "sleazy"
          "shaky" "runner" "riddle" "ranger" "pickup" "nutty" "menace" "inspiring"
          "housekeeper" "fingernail" "chronic" "baking" "whine" "utter" "strap" "sniff"
          "sedative" "picket" "jumbo" "hound" "homosexual" "flatter" "dwell" "assignment"
          "advisor" "unreasonable" "souvenir" "psychopath" "operative" "obstruction"
          "lockdown" "gloat" "filth" "exterminate" "electronic" "cedar" "betraying"
          "appealing" "wrath" "trainer" "publicly" "marshal" "heavenly" "employed"
          "doggie" "diplomatic" "dilemma" "contagious" "cheering" "carved" "wrench"
          "thingy" "rogue" "piggy" "pageant" "liveliness" "gardener" "continuing" "squat"
          "motivation" "introduction" "feminine" "eyeball" "convertible" "context"
          "suspense" "poppy" "incompetent" "differ" "copper" "sensitivity" "prophecy"
          "immortal" "colourless" "asking" "demonstrate" "acquaintance" "superficial"
          "ingredient" "bruised" "apparent" "worthwhile" "impatient" "arctic" "profound"
          "mocking" "concentration" "tribute" "postcard" "judgement" "engineering"
          "commonwealth" "intimidate" "doorbell" "cultural" "café" "allergy"
          "vegetarian" "telegram" "pharmacy" "obstacle" "contribute" "bankrupt"
          "neurotic" "microphone" "persuasive" "lighthouse" "liability" "detailed"
          "condolences" "await" "ambush" "adolescent" "abort" "verse" "undermine"
          "sunlight" "slippers" "nonetheless" "liaison" "encouragement" "dictate"
          "blueberry" "apron" "anticipated" "vouch" "sibling" "magically" "listener"
          "fearless" "dinosaur" "repeatedly" "moody" "indulge" "horribly" "expand"
          "dialogue" "adventurous" "violin" "suffice" "oatmeal" "midst" "maiden"
          "damaging" "boredom" "sunglasses" "prognosis" "molecular" "hazel" "striking"
          "plumber" "newborn" "swift" "speculation" "rewrite" "handicapped" "dishonest"
          "amusement" "unborn" "toaster" "radioactive" "questionable" "overlook"
          "underpants" "stool" "platinum" "nightclub" "minority" "divided" "dangerously"
          "considerable" "tuition" "godmother" "crude" "criticize" "whichever"
          "correction" "incentive" "yogurt" "submarine" "startle" "interpretation"
          "ingoing" "enchant" "conservative" "bandage" "awareness" "supervision" "sedate"
          "marker" "brainwash" "adamant" "uncommon" "medieval" "maturity" "maternity"
          "marinade" "candid" "snore" "sneeze" "smuggle" "salty" "morbid" "licence"
          "defenceless" "cradle" "booking" "traumatize" "muddy" "grapefruit" "bossy"
          "bacteria" "speechless" "parachute" "cremate" "transaction" "polar" "intrude"
          "ignorance" "handyman" "greens" "diabete" "constitutional" "pronoun" "pottery"
          "obscene" "induce" "immigration" "elevate" "disable" "crust" "conductor"
          "assessment" "withdrawal" "undone" "symptom" "mascara" "iceberg" "convert"
          "complicate" "combine" "transformation" "telescope" "stumble" "saliva" "robber"
          "reconnect" "irregular" "invalid" "evict" "diver" "cushion" "rhyme"
          "phenomenon" "motherhood" "migraine" "frosty" "friction" "aspect" "aggression"
          "abusive" "trivial" "tenant" "supervise" "superstitious" "statistics"
          "homemade" "glamour" "disoriented" "crossroad" "assurance" "thesis" "terrorism"
          "respectfully" "painkiller" "intellect" "amuse" "alienate" "unwanted" "soften"
          "plaster" "insomnia" "foremost" "deviate" "crave" "craft" "conceal" "abdomen"
          "toenail" "organism" "incapable" "erotic" "dwelling" "calory" "breathtaking"
          "unattractive" "representative" "residential" "resignation" "substantial"
          "agricultural" "arbitrary" "detached" "dictator" "digestion" "discourage"
          "discrimination" "disgraceful" "disguise" "disorganized" "diversity" "download"
          "compensate" "competence" "earnest" "eclipse" "economical" "elastic"
          "electronics" "embargo" "encyclopaedia" "energetic" "enjoyment" "enlargement"
          "envious" "equator" "erect" "expenditure" "eyelid" "farsighted" "feminist"
          "forfeit" "furthermore" "futile" "hazardous" "immortality" "initially"
          "intricate" "juvenile" "kaleidoscope" "magnify" "miscellaneous" "momentary"
          "mutter" "nylon" "orient" "ornament" "oyster" "parasol" "pasture" "pioneer"
          "playful" "poisonous" "practitioner" "precision" "prefix" "prestige"
          "punctuation" "quarrel" "raffle" "rationalize" "relay" "rugby" "scallop"
          "spank" "specialize" "stereotype" "complement" "completely" "completion"
          "complimentary" "component" "composition" "comprehension" "conceited"
          "connoisseur" "consequently" "conservation" "considerably" "consistency"
          "consonant" "conspicuous" "constitute" "consumer" "consumption" "contemplate"
          "contemporary" "contented" "continuous" "contradict" "contradiction" "contrast"
          "controversial" "controversy" "conventional" "copier" "cosmopolitan"
          "counterfeit" "countryside" "crayon" "cricket" "culprit" "cupboard"
          "curriculum" "customs" "cylinder" "interim" "locum" "stride" "subjective"
          "submission" "subordinate" "subscription" "subtract" "suburb" "supplement"
          "supporter" "swimsuit" "syllable" "syllabus" "abbreviate" "abode" "abolish"
          "abound" "abrupt" "abstinence" "accommodate" "accord" "accumulate" "accustom"
          "acquaint" "adhere" "adjective" "administer" "admissible" "adverb" "adversary"
          "affability" "affirm" "agrarian" "walnut" "watery" "wharf" "whirl" "wholly"
          "wilful" "willingness" "wireless" "withhold" "wrangle" "wring" "vaccination"
          "vandalism" "vapour" "varnish" "velocity" "venison" "venomous" "verification"
          "versatile" "vestige" "vibrate" "vigilance" "vigorous" "visibility" "vogue"
          "volleyball" "vulgar" "vulture" "ulcer" "unaccustomed" "unavoidable" "unaware"
          "unbutton" "unchanged" "undeniable" "unequal" "uneven" "unfamiliar"
          "unfriendly" "unjust" "unkind" "unruly" "unrest" "unsettled" "unskilful"
          "untidy" "unworthy" "upbrining" "uphill" "uprising" "usage" "utmost" "tangible"
          "tariff" "tea-pot" "teaspoon" "tedious" "temporal" "tendon" "testament"
          "test-tube" "textile" "text-message" "theological" "thought" "threshold"
          "thrift" "timid" "tonsil" "torrent" "tortoise" "trait" "transient" "treason"
          "tread" "trickle" "Trinity" "trolley" "turban" "turbulent" "turnip" "twirl"
          "twitter" "tyran" "typist" "allege" "allot" "alloy" "allude" "allusion" "aloud"
          "alphabet" "altitude" "aluminium" "amass" "amnesty" "amorous" "anaemia"
          "analogy" "anatomy" "animate" "animosity" "annex" "anomalous" "anthem"
          "ant-hill" "antipathy" "antiquarian" "apathetic" "apostle" "apparatus"
          "appease" "appendix" "appoint" "appraise" "apprehension" "apprentice"
          "archbishop" "archives" "ardent" "aristocracy" "armchair" "armistice" "armpit"
          "aroma" "arouse" "arrears" "artful" "ascend" "ascertain" "ashore" "asparagus"
          "aspiration" "assent" "assert" "assimilate" "astray" "atheism" "atrocious"
          "audible" "austerity" "auxiliary" "avalanche" "aversion" "aviation" "bamboo"
          "barley" "barracks" "barren" "barricade" "barter" "beech" "beetroot"
          "benediction" "bereave" "berch" "bilateral" "birch" "bladder" "bleat" "blister"
          "bonfire" "bonnet" "botany" "box-office" "boycott" "boyish" "brisk" "brooch"
          "brood" "bulletin" "buoyancy" "calamity" "camomile" "canary" "candied"
          "canteen" "canvass" "capability" "caprice" "captivity" "caravan" "cardinal"
          "caress" "cartrige" "cataract" "caterpillar" "cathedral" "cauliflower"
          "catastrophe" "cavalry" "celery" "census" "cessation" "chaste" "chatter"
          "chirp" "chisel" "chivalrous" "chord" "cipher" "clang" "clasp" "clatter"
          "clench" "clove" "clumsiness" "clutch" "coarse" "cobbler" "cod-liver"
          "cognition" "collaborate" "combustible" "comet" "commence" "commotion" "compel"
          "compile" "compliance" "compose" "compress" "comprise" "consession"
          "conciliate" "concord" "condensed" "confederate" "confer" "confiscate"
          "conform" "conjecture" "conjunction" "consensus" "consignment" "consternation"
          "constituent" "constrain" "construe" "consume" "contend" "contentment"
          "continual" "contraceptive" "contrivance" "convey" "co-operate" "co-ordinate"
          "coral" "coronation" "correspond" "countess" "crater" "crayfish" "crease"
          "creche" "credulous" "crescent" "crest" "croak" "crochet" "crockery" "crumple"
          "crusade" "cudgel" "currant" "cutlery" "cutlet" "cyber" "cypress" "daffodil"
          "dainty" "dandelion" "dangle" "debit" "deceit" "deduce" "default" "defer"
          "defiance" "deficiency" "deform" "defrost" "degenerate" "degradation" "deity"
          "dejection" "delicacy" "deluge" "demolish" "demure" "deplorable" "deport"
          "deprivation" "deride" "desirable" "desolate" "despot" "deter" "detergent"
          "detriment" "devolution" "diarrhoea" "dilate" "diligence" "diminish"
          "disapproval" "disbelief" "discard" "disciple" "discomfort" "discredit"
          "dismal" "dispatch" "dissatisfaction" "domicile" "dormitory" "dowry" "drape"
          "drizzle" "drone" "drought" "drowsy" "Dutch" "dynasty" "dysentery" "earnings"
          "easel" "edible" "egoism" "embankment" "embark" "embroider" "empower" "endorse"
          "enrich" "entail" "envisage" "escalator" "evasion" "excel" "exempt" "exhale"
          "expedient" "expend" "extinct" "fable" "faciliate" "falcon" "fastidious"
          "fathom" "feeble" "ferocious" "fibre" "fiddle" "fillet" "flake" "flask"
          "flippers" "flourish" "flute" "foliage" "fortress" "frenzy" "funnel" "gills"
          "giraffe" "girdle" "gloss" "go-between" "grope" "guile" "haphazard" "harness"
          "hearth" "hedge" "heritage" "heron" "herring" "highjack" "hurdle" "hybrid"
          "icicle" "ignoble" "illuminate" "imaginable" "immense" "imminent" "immobile"
          "immoderate" "impatience" "impossibility" "imprison" "improbable" "imprudence"
          "impure" "inaccessible" "inaccurate" "inaudible" "inaugurate" "inclination"
          "incomparable" "incompatible" "incomprehensible" "indecent" "indictment"
          "indifference" "indigestion" "infantry" "inflame" "inflict" "influenza"
          "ingenious" "inhabit" "inhuman" "innumerable" "inoculate" "inoffensive"
          "in-patient" "inquire" "inscribe" "insensible" "inseparable" "insolence"
          "instalment" "insufficient" "insular" "insulation" "interact" "intermediary"
          "intolerable" "intoxicate" "invaluable" "invariable" "irreparable"
          "irriplaceable" "jackal" "jolly" "jubilee" "judicial" "judicial" "junction"
          "juggle" "justification" "languish" "lateral" "lattice" "leaden" "leech"
          "livelihood" "loneliness" "madden" "maggot" "malignant" "malnutrition"
          "mandate" "manhood" "manoeuvre" "manpower" "marvel" "mayonnaise" "measles"
          "mediate" "melancholy" "minimize" "miscalculate" "mischief" "misty" "misuse"
          "monastery" "mortify" "multicultural" "mutton" "navigable" "nitrogen"
          "nobility" "non-stop" "novice" "noxious" "nutshell" "obstinacy" "obstruct"
          "odious" "odour" "offhand" "onset" "onward" "opaque" "optical" "orator"
          "orchid" "ostrich" "padlock" "pagan" "palate" "partridge" "peacock" "pedigree"
          "penetrate" "peril" "perish" "perpetual" "perseverance" "pheasant" "pickpocket"
          "placard" "placid" "plank" "plough" "pluck" "poach" "pollute" "porcelain"
          "porous" "Portuguese" "potent" "poultry" "precarious" "predicament" "preface"
          "prehistoric" "premium" "preoccupation" "preside" "primrose" "probability"
          "procure" "prodigy" "prolific" "prolong" "prone" "propaganda" "propulsion"
          "prose" "protrude" "province" "proximity" "prudence" "prunes" "psalm"
          "pullover" "radiance" "radish" "raincoat" "ramble" "rapture" "ratification"
          "readiness" "recoil" "recollect" "reconstruct" "rectangle" "rectify"
          "recycling" "redeem" "refine" "refuel" "refusal" "relapse" "relent"
          "reluctance" "reluctance" "remnant" "render" "repetition" "reprint" "reptile"
          "repulse" "requisite" "respiration" "revision" "revolt" "ripple" "roadmap"
          "roadside" "rotate" "rouge" "salutation" "sanctify" "sanitary" "saturate"
          "savour" "sceptical" "sculptor" "seaman" "seashore" "selfconfidence"
          "selfcontrol" "selfemployed" "selfevident" "selfportrait" "semicircle" "sermon"
          "serpent" "shabby" "shallow" "shawl" "sickly" "sinister" "sinner" "slander"
          "sledge" "sleek" "slogan" "sociable" "son-in-law" "soothe" "sovereign"
          "Spaniard" "spanner" "sparrow" "speck" "spinster" "spire" "squint" "stair"
          "stallion" "stammer" "staple" "starch" "starvation" "stingy" "strategic"
          "sturgeon" "subdue" "submerge" "sulphur" "surpass" "susceptible" "swarm"
          "Swede" "Swiss" "synonym" "bastard" "enjoying" "pleased" "burns" "commissioner"
          "vanquish" "morgue" "autopsy" "freely" "racket" "overdue" "lacking" "crock"
          "clamp" "canned" "bathtub" "artery" "warmer" "ravine" "pushy" "laughter"
          "grove" "fastest" "carrier" "auntie" "wiser" "willingly" "waltz" "thinner"
          "swelling" "steroid" "slate" "sentinel" "rookie" "rehearse" "quarterback"
          "ledge" "justified" "hateful" "doorway" "admiral" "wrestle" "velvet" "manor"
          "madman" "intriguing" "idiotic" "enlighten" "donut" "brunch" "bridesmaid"
          "barrister" "architect" "applause" "alongside" "wretched" "smoothly" "plead"
          "payoff" "jumpy" "intervention" "hanging" "freed" "flashing" "convent"
          "captive" "vanity" "skate" "preview" "perjury" "parental" "onboard" "mugged"
          "maestro" "linen" "gypsy" "grind" "greasy" "estimate" "elementary" "drastic"
          "dolly" "compatriot" "cocky" "sprung" "spotlight" "revealing" "racist"
          "preacher" "piper" "overly" "locket" "hover" "endure" "chained" "booty"
          "scrambled" "rattle" "linked" "investor" "hustle" "forensic" "enthusiastic"
          "devastating" "democrat" "comeback" "cheaper" "charter" "calling" "blushing"
          "wrecking" "waffles" "virginity" "uninvited" "unfaithful" "underwater"
          "redhead" "ongoing" "lesser" "jingle" "intellectual" "installed" "indefinitely"
          "genetically" "fireman" "faculty" "discretion" "declaration" "crate"
          "competent" "breaker" "bakery" "asylum" "wedge" "unfit" "tuxedo" "torment"
          "superhero" "stirring" "spinal" "server" "seminar" "rabble" "pneumonia"
          "override" "manslaughter" "lettuce" "kinky" "instructor" "grieve" "gorilla"
          "frustration" "extensive" "exploration" "authentic" "arraignment" "tighter"
          "suitable" "specimen" "solving" "overprotective" "identification" "grandchild"
          "genuinely" "founded" "flushed" "floss" "decorated" "crunch" "cramp" "corny"
          "connecting" "bitten" "ultrasound" "ultimatum" "retrieve" "multi" "millionaire"
          "mechanical" "hiking" "hatchet" "doubtful" "dedication" "cellular" "butterfly"
          "biopsy" "voluntarily" "ventilator" "unload" "universal" "snitch" "reassure"
          "mystical" "mayhem" "matrimony" "magnet" "curly" "cupid" "comrade" "bragging"
          "aircraft" "adjusted" "vaguely" "tying" "slash" "setback" "retail" "melting"
          "felicity" "expelled" "decoration" "blackout" "believable" "backfire" "vista"
          "vintage" "slimy" "renting" "reign" "mailbox" "informant" "disconnect"
          "designing" "crooked" "crook" "conveniently" "colon" "tacky" "steering"
          "stability" "reset" "radius" "opener" "festive" "desperation" "critic"
          "consulting" "bagel" "authorization" "agitate" "wishful" "unbearable" "tonic"
          "suction" "sporting" "safest" "newlywed" "nauseous" "misguided" "mildly"
          "liable" "introducing" "dislike" "diploma" "delude" "crummy" "contraction"
          "carve" "bottled" "unavailable" "twenties" "trustworthy" "stupidity" "remorse"
          "preferably" "photography" "outsider" "operational" "northwest" "mourn"
          "mechanism" "mafia" "Greece" "greatness" "girlie" "delirious" "cubicle"
          "check-up" "certified" "automobile" "athletic" "windscreen" "viper" "starring"
          "spear" "serenity" "quarry" "prosecutor" "probe" "potentially" "poodle"
          "pitiful" "persistent" "perception" "neighbourhood" "nagging" "masterpiece"
          "limbo" "karate" "irritating" "incline" "holler" "gauge" "fiasco" "fallout"
          "educational" "destination" "crimson" "continent" "cider" "brochure"
          "behaviour" "bargaining" "arena" "wiggle" "welcoming" "villain" "smear"
          "resentment" "penguin" "panther" "overhear" "mounted" "morality" "leopard"
          "jackpot" "handshake" "grilled" "formality" "elevator" "bypass" "breeding"
          "boxer" "binding" "audio" "accidental" "ulterior" "tangle" "sought" "softball"
          "smallest" "sling" "rumour" "remarried" "reluctant" "puddle" "perceptive"
          "miraculous" "memorable" "maternal" "lookout" "longing" "lockup" "lizard"
          "librarian" "junkie" "immoral" "hypothetically" "extortion" "essentially"
          "downright" "digest" "cranberry" "covert" "chorus" "bygone" "buzzing" "weary"
          "viewer" "uptown" "tucker" "transmitter" "taping" "takeout" "stepmother"
          "stale" "merger" "mandatory" "ludicrous" "inject" "deceiving" "caterer" "budge"
          "vending" "sexist" "sanctuary" "regain" "processing" "picky" "misjudge"
          "miscarriage" "memorize" "licking" "jitter" "invade" "interruption" "illegally"
          "glitch" "fewer" "doubly" "distraught" "dispose" "dagger" "cruelty"
          "cancelling" "belonging" "allegation" "alias" "aging" "zomby" "slaughter"
          "sensational" "revolutionary" "overhead" "oddly" "interrogate" "imperative"
          "impeccable" "hurtful" "helper" "endangered" "dungeon" "disgust" "devious"
          "destruct" "countdown" "brotherhood" "brink" "berry" "banker" "ballroom"
          "annoyance" "admirer" "admirable" "activate" "weaver" "valve" "trout" "scrap"
          "resourceful" "remarkably" "refresh" "puffy" "precaution" "pointy" "nipple"
          "ministry" "hubby" "flare" "fierce" "demise" "crushing" "clinging" "checkbook"
          "cashmere" "calmly" "believer" "amazingly" "ultra" "tolerance" "tactical"
          "stairwell" "slower" "sewing" "separately" "restricted" "partially" "mingle"
          "knack" "gullible" "folding" "financially" "filming" "eater" "dysfunctional"
          "dripping" "ditto" "distinguished" "defence" "defeated" "corruption"
          "contractor" "conceive" "clone" "circulation" "caliber" "brighten" "banquet"
          "anticipate" "annoy" "volatile" "successfully" "severely" "runaway"
          "quarantine" "premiere" "painless" "orphan" "orphanage" "offence" "oasis"
          "negotiation" "narcotic" "mistletoe" "meddling" "manifest" "intrigue"
          "injustice" "homicidal" "granny" "gigantic" "disturbance" "disastrous"
          "demented" "cosmic" "cheerful" "brunette" "beverage" "arcade" "unethical"
          "swollen" "scalpel" "prototype" "prescribe" "pompous" "poetic" "manipulation"
          "lasting" "insignificant" "inmate" "hasty" "grumpy" "fulfilled" "evolution"
          "discharged" "disagreement" "cornered" "copied" "confrontation" "brightest"
          "banned" "attendant" "athlete" "amaze" "stuffy" "sexuality" "segment"
          "revelation" "respirator" "pulse" "processed" "polygraph" "ordinarily"
          "morally" "matrix" "martyr" "martial" "invaded" "homey" "heartbroken" "groove"
          "greatly" "forge" "florist" "firsthand" "fiend" "expanding" "drummer" "dandy"
          "crippled" "craving" "connote" "conditioner" "bubbly" "beeper" "baptism"
          "wiring" "tummy" "surrogate" "stray" "slowing" "scoot" "scanner" "rightful"
          "richest" "prophet" "paralysis" "mellow" "makeover" "interstate" "historic"
          "flock" "disgusted" "broadsheet" "borrowing" "wildest" "unauthorized" "troll"
          "snatch" "retro" "quilt" "painfully" "outlet" "mainland" "lonesome" "lawfully"
          "intercept" "hector" "hamster" "grownup" "goldfish" "glued" "ghetto" "delusion"
          "compelling" "colonial" "charitable" "carton" "boiling" "awaiting" "assign"
          "arrogance" "takeover" "synchronize" "stalk" "spleen" "spacecraft" "prominent"
          "prise" "presumptuous" "prejudice" "platoon" "pickle" "permitted" "mummy"
          "macaroni" "legendary" "itinerary" "hepatitis" "heave" "gender" "fading"
          "dumbest" "dishwasher" "descriptive" "cunning" "cripple" "compulsive"
          "burglary" "bumpy" "blaze" "affirmative" "adrenaline" "unreal" "taint" "spree"
          "sever" "scarce" "scalp" "rewind" "pretentious" "planner" "placing" "overrated"
          "loathe" "joker" "heater" "gospel" "filter" "fertility" "exterior" "epidemic"
          "enterprise" "emerald" "ecstatic" "ecstasy" "distribution" "clubhouse"
          "cheater" "bursting" "breather" "bending" "attendance" "arsonist" "valiant"
          "uphold" "unarmed" "thrilling" "thigh" "terminate" "spiral" "spaceship" "salon"
          "quaint" "patronize" "patio" "paddle" "mailman" "joyous" "invincible"
          "interpret" "insecurity" "infamous" "fragment" "exploit" "dusty" "countless"
          "conjure" "confine" "chateau" "bleach" "backseat" "accomplishment" "wisely"
          "wildlife" "valet" "vaccine" "unnatural" "tasting" "seducing" "secretive"
          "screwdriver" "rightfully" "poorly" "polling" "pedestal" "mystic" "miniature"
          "microscope" "maple" "lantern" "infinite" "incriminate" "hygiene" "freight"
          "flooding" "eliminated" "deacon" "cuter" "continental" "container" "cavity"
          "Capricorn" "atomic" "wider" "dumpling" "underage" "truthfully" "tamper"
          "statute" "sparkling" "socially" "sideline" "rhinoceros" "railing" "puberty"
          "phantom" "pesky" "outrage" "openly" "nominate" "itching" "intuition"
          "humility" "fishy" "finch" "ferry" "excessive" "evolve" "eligible" "dosage"
          "disrupt" "dipping" "derange" "cuckoo" "craziness" "compatible"
          "circumstantial" "bunker" "asthma" "arise" "triad" "trashy" "thorn" "suburban"
          "soothing" "slumber" "slayer" "shindig" "sentiment" "riddance" "purity"
          "offshore" "massacre" "isolation" "impersonal" "hamlet" "footprint" "fluke"
          "festivity" "feisty" "evacuate" "detain" "defender" "creeping" "classics"
          "bitterness" "baloney" "ashtray" "apocalypse" "zillion" "viable" "sympathize"
          "squirt" "spade" "snappy" "sleepover" "reunited" "retainer" "restroom"
          "repercussion" "relive" "reconciliation" "reconcile" "recognise" "raven"
          "prevail" "preach" "noose" "meatloaf" "manicure" "landlady" "hypothesis"
          "homesick" "hectic" "heartbreak" "haunting" "frown" "fingerprint" "extract"
          "expire" "exhausting" "exceptional" "disregard" "crisp" "cooperative" "cling"
          "blender" "bitty" "badger" "anticipation" "advocate" "waterfront" "upstanding"
          "unprofessional" "unhealthy" "turmoil" "truthful" "toothpaste" "thoughtless"
          "spook" "shady" "senseless" "scooter" "ringer" "refuge" "preposterous"
          "portable" "pastry" "loner" "jogging" "itchy" "insinuate" "hospitality"
          "forthcoming" "excess" "etiquette" "ending" "distinction" "desist" "deprive"
          "cloak" "casserole" "beaver" "bearer" "applaud" "appalling" "trump" "trench"
          "touchdown" "tailor" "swoop" "sticker" "smite" "shameless" "researching"
          "reclaim" "purposely" "proxy" "pineapple" "parasite" "offspring" "multiply"
          "masculine" "gunfire" "furnace" "flyer" "engrave" "duplicate" "designate"
          "delicatessen" "cryptic" "creed" "condemn" "colossal" "clipper" "cliché"
          "clarity" "banish" "argon" "unicorn" "uncanny" "treasury" "technicality"
          "static" "scales" "satin" "rosebud" "relentless" "rearrange" "plunge"
          "obsolete" "mindless" "lullaby" "layout" "grown up" "flashy" "encode" "dread"
          "doodle" "dealing" "cupcake" "console" "conclusive" "bowel" "announcing"
          "abide" "wonderfully" "warfare" "violate" "suicidal" "skilled" "sketchy"
          "shoplifting" "quizmaster" "profitable" "politically" "needless" "momentarily"
          "midget" "mercury" "leukemia" "fungus" "extraction" "explorer" "eighty"
          "chimera" "cheery" "cadet" "benign" "artillery" "apiece" "abduction" "virtual"
          "unspeakable" "unlimited" "unidentified" "twinkle" "transcript" "surfing"
          "stricken" "stork" "sodium" "snick" "retrieval" "quickie" "playboy" "observer"
          "mausoleum" "kangaroo" "intensive" "infest" "incompetence" "horrified" "grunt"
          "fracture" "formerly" "fireball" "firearm" "examiner" "elite" "dashing" "curry"
          "crumb" "crash down" "courier" "conclude" "cockroach" "boulevard" "bookie"
          "baptize" "astronaut" "allegiance" "aiming" "workplace" "weave" "weaker"
          "suffocate" "stench" "stark" "spawn" "sideway" "shortcut" "repulsive" "provoke"
          "penitentiary" "milligram" "midge" "lapse" "knuckle" "intercourse" "improvise"
          "implant" "hometown" "handicap" "giddy" "garland" "gallant" "furry" "fruitful"
          "footing" "finding" "editorial" "discovering" "detour" "cuddle" "coordinate"
          "collector" "bailiff" "auditioning" "aching" "usher" "typically" "topless"
          "squid" "snowman" "sensor" "seller" "renown" "reflex" "recruiting" "raisin"
          "racial" "pyramid" "preservation" "portfolio" "pilgrim" "patriot" "oversight"
          "merciful" "magistrate" "intimidating" "infirmary" "inconvenient" "imposter"
          "godforsaken" "forgery" "foolproof" "folder" "flattery" "fingertip"
          "exterminator" "eccentric" "dodgy" "constructive" "compartment" "bodily"
          "armour" "alimony" "accustomed" "wallow" "vicinity" "venue" "upgrade"
          "upcoming" "untrue" "uncover" "trembling" "taunt" "strand" "seizure" "savvy"
          "revive" "retard" "recruit" "rationally" "provenance" "prestigious" "perky"
          "pedal" "overdose" "nasal" "mushy" "mover" "melodramatic" "medic" "manure"
          "magnetic" "knockout" "knitting" "hotline" "hammock" "framing" "flannel"
          "existing" "eavesdrop" "dwarf" "doggy" "constellation" "collision" "bleak"
          "blacked" "batter" "archer" "aggravate" "vocal" "unwind" "twitch" "taxpayer"
          "reinforce" "dictation" "dilute" "disintegrate" "disposable" "disqualify"
          "dissolve" "distinguish" "doctrine" "dolphin" "dominant" "dominate" "downward"
          "drench" "dynamic" "earlobe" "ebony" "ecology" "egotistical" "elated" "elitist"
          "eloquent" "emphasize" "enjoyable" "Fahrenheit" "flirtatious" "gymnasium"
          "hallucinate" "incorporate" "increasingly" "jigsaw" "locomotive"
          "materialistic" "parliamentary" "parlour" "pastel" "precipitation" "predicate"
          "prediction" "prefecture" "procession" "prohibit" "Protestant" "quotation"
          "realization" "recession" "recreation" "reorganize" "repetitious"
          "reproduction" "whereby" "saxophone" "scallion" "sesame" "aback" "abash"
          "abate" "abdicate" "abject" "absolve" "abstain" "abundance" "acclaim" "accrue"
          "accuracy" "acerbity" "acorn" "acquit" "acrid" "adjacent" "adjoin" "admonish"
          "adroit" "adultery" "aerial" "affinity" "affliction" "affront" "aghast"
          "heather" "zoology" "yeast" "yearn" "whimper" "whirlpool" "willow" "wistful"
          "wistful" "wreath" "wrest" "wriggle" "vehemence" "vexation" "vivacious"
          "voracious" "udder" "up-to-date" "tapestry" "tardy" "thimble" "throng" "thump"
          "thwart" "tinge" "tinsel" "tongs" "topple" "transgress" "treble" "trifle"
          "truant" "tumult" "twine" "alight" "alignment" "alleviate" "amiable"
          "annihilate" "antler" "arable" "asunder" "audacious" "augment" "bayonet"
          "beehive" "beseech" "besiege" "blemish" "blunder" "boisterous" "bough" "brazen"
          "brevity" "bristle" "brittle" "broth" "buckle" "bulging" "callous" "caprize"
          "caraway" "cloister" "coexist" "confound" "countenance" "covetous" "cower"
          "crevice" "crutch" "cumbersome" "decadence" "depict" "depot" "depreciate"
          "derivative" "destitute" "deterrent" "devaluation" "devise" "devoid" "devour"
          "devout" "diffident" "digress" "dimple" "disarm" "discern" "discontent"
          "discord" "disdain" "disembark" "disfigure" "dishearten" "dishonour"
          "disillusion" "disinterested" "dismantle" "dismount" "dispense" "disperse"
          "displace" "displease" "disquiet" "dissipate" "distil" "distort" "distrust"
          "diverge" "doleful" "droop" "dropper" "drudgery" "drunkard" "dubious" "dummy"
          "durable" "edifice" "efface" "effeminate" "efficacious" "Egyptian" "elapse"
          "elegance" "elusive" "embezzlement" "emblem" "embody" "embryo" "emigrant"
          "eminence" "emptiness" "enact" "enamel" "encircle" "enclose" "encroach"
          "encumber" "endeavour" "endurance" "enlist" "enliven" "enmity" "enrage"
          "enrapture" "ensue" "entangle" "entice" "entrails" "entreat" "entrust"
          "enumerate" "enviable" "envoy" "epoch" "equanimity" "equilibrium" "eradicate"
          "evade" "evaporate" "exalt" "excursion" "exemplify" "expulsion" "exaltation"
          "falter" "feign" "ferment" "fervent" "fidelity" "fidget" "fiery" "Finnish"
          "fleeting" "flicker" "flimsy" "flippant" "flint" "flounder" "flutter" "fodder"
          "folly" "forbear" "forefather" "forestall" "formidable" "forsake" "frail"
          "frantic" "freckless" "frock" "frontier" "froth" "frugal" "furrow" "furtively"
          "gaiety" "gallop" "gargle" "gaunt" "gauntlet" "gauze" "gelatine" "genial"
          "germinate" "ghastly" "giddiness" "gland" "glaze" "gleam" "glide" "glimmer"
          "glisten" "glitter" "glorify" "granary" "granite" "granulated" "gratification"
          "grating" "gravitation" "graze" "grimace" "gruff" "grumble" "gurgle"
          "haberdashery" "haemorrhage" "haggard" "harmonious" "harrow" "haughty" "haunch"
          "heartburn" "heathen" "hinge" "hoard" "hoarfrost" "hosiery" "immovable"
          "impediment" "impel" "impending" "imperial" "impertinent" "imperturbable"
          "impetus" "implore" "improper" "impunity" "inability" "inadequate"
          "inadmissible" "inborn" "inconceivable" "inconsistency" "indebted" "indecision"
          "indefinite" "indignant" "indispensable" "indisputable" "indistinct"
          "ineffective" "inefficient" "inert" "inestimable" "inexhaustible" "inexorable"
          "infallible" "infatuated" "infer" "inferior" "inflate" "influx" "infringement"
          "infuriate" "ingratitude" "injunction" "inopportune" "insoluble" "intrepid"
          "involuntary" "iodine" "irksome" "irreproachable" "irrigate" "jack-knife"
          "jagged" "javelin" "jovial" "judicious" "kernel" "kindle" "know-how" "lacquer"
          "laden" "ladle" "lament" "latch" "lathe" "lather" "laurel" "lavender" "lilac"
          "lime-tree" "linger" "locust" "louse" "lubricant" "lukewarm" "luminous"
          "luscious" "lustre" "luxuriant" "magpie" "mammoth" "manuscript" "marten"
          "melon" "militant" "mimic" "mince" "mirth" "mitten" "morsel" "montley" "muffle"
          "mulberry" "mumble" "mural" "musty" "mutilate" "muzzle" "nautical" "neigh"
          "nettle" "niche" "nimble" "nominal" "noncommittal" "nondescript"
          "notwithstanding" "oblique" "oblong" "oppress" "ostentatious" "otter" "quail"
          "quaver" "quiver" "paltry" "pansy" "parch" "parish" "parson" "partition"
          "patter" "patty" "pebble" "penal" "pendulum" "pensive" "peppermint" "perch"
          "perplex" "persecute" "perverse" "phonebooth" "physique" "pimple" "pitfall"
          "plaice" "plaid" "plaintiff" "plait" "pleat" "plentiful" "pliable" "plight"
          "plunder" "plywood" "polarity" "ponder" "poplar" "posterity" "potter" "pouch"
          "pounce" "prelude" "prerogative" "pretence" "pretext" "prism" "prologue"
          "propel" "proprietary" "proprietor" "provocation" "pulverize" "pungent"
          "ragged" "rascal" "ravage" "rebuff" "rebuke" "recline" "recognition"
          "recompense" "reconnaissance" "refrain" "refutation" "regent" "rejoice"
          "relinquish" "remittance" "repent" "repress" "reprimand" "reproach" "reproof"
          "resolute" "resound" "respite" "restoration" "resurrection" "retaliate"
          "reticence" "retinue" "retort" "rhubarb" "rivet" "rouse" "rowan" "rudder"
          "ruffian" "rumble" "rummage" "rustic" "rustle" "sable" "sacrament" "sagacious"
          "sardine" "satire" "scaffolding" "scald" "scanty" "scorch" "scoundrel" "scowl"
          "scribble" "scrupulous" "scythe" "seclude" "seethe" "selfservice" "semblance"
          "sequel" "shaggy" "shear" "sherry" "shingle" "shrill" "shrivel" "shrub"
          "shudder" "shuffle" "sickle" "siege" "silliness" "sinew" "singe" "skipper"
          "skirmish" "sluice" "slush" "smother" "smoulder" "snarl" "sneer" "snort"
          "snout" "solder" "soloist" "soluble" "spatter" "spectrum" "spinach" "spittle"
          "splinter" "sprinter" "squeak" "stagger" "standpoint" "staunch" "steed"
          "steeple" "steppe" "sterile" "steward" "stifle" "stipulate" "stocking" "stoker"
          "stout" "strenuous" "strew" "stripe" "strive" "stubble" "stump" "stupendous"
          "sturdy" "stutter" "subdivide" "subside" "subsist" "substance" "suede" "sullen"
          "sultry" "sumptuous" "sunken" "superfluous" "superintend" "superintendent"
          "supersede" "supplant" "supple" "suppress" "surmise" "surmount" "swarthy"
          "swerve" "swindle" "swine" "synthesis" "zealot" "zealous" "zenith" "zigzag"
          "zodiac" "abattoir" "abattoir" "abbot" "abberance" "abberant" "abhor"
          "abhorrence" "abhorrent" "abjection" "abjure" "ablative" "ablaze" "able-bodied"
          "abnegate" "abnegation" "abnormality" "abolishment" "aboninable" "abonnement"
          "aboriginal" "aborigines" "abortive" "above-average" "aboveboard" "abrasive"
          "abreast" "abridge" "abruptness" "absently" "absolution" "absorbing" "abyss"
          "accede" "acceleration" "accolade" "accommodating" "accompaniment" "accordance"
          "accredited" "accuser" "acheless" "acquisition" "actual" "adaptation"
          "addressee" "adept" "adherence" "admiration" "admonishment" "adornment"
          "adulate" "adulatory" "adulthood" "advancement" "adverse" "adversity"
          "advisedly" "advocacy" "affable" "affluence" "affluent" "afloat" "afterthought"
          "aggregate" "agreeable" "ailment" "aimless" "air-condition" "airless"
          "air-tight" "alacrity" "albeit" "alcove" "alertness" "allay" "allocate"
          "all-purpose" "allure" "aloof" "alteration" "ambient" "ambiguity" "amendable"
          "amenity" "amoral" "amorphous" "amplify" "amplitude" "anchovy" "angrily"
          "anguish" "angular" "annotation" "anoitment" "antagonize" "antarctic"
          "antecedent" "antenna" "anteroom" "antisocial" "antiterrorist" "apathy"
          "aplenty" "apostrophe" "appal" "appellation" "append" "appetizing" "applicant"
          "appraisal" "approbation" "aptitude" "aptly" "archaic" "archery" "archly"
          "ardently" "arduous" "armament" "armature" "armless" "armory" "array"
          "articulate" "artless" "artsy" "ascension" "askew" "asperity" "aspirant"
          "assail" "assort" "assortment" "assuage" "assumptive" "assuredness" "astound"
          "atonement" "atrocity" "attachment" "attentive" "attenuate" "attest"
          "attestation" "attire" "attrition" "attune" "aubergine" "audacity" "audibly"
          "auditory" "auspice" "avenge" "avert" "avouch" "aweless" "backache" "baggy"
          "bagpipe" "ballot" "banal" "banter" "baseless" "bashful" "basin" "bazaar"
          "beastly" "beautify" "bedding" "bedevil" "bedridden" "beefy" "befriend"
          "beggarly" "begrudge" "bellow" "benefactor" "benevolence" "benevolent"
          "benignity" "berth" "berth" "beset" "bestial" "bestiality" "bestow" "bewitch"
          "big-hearted" "bigot" "bilingual" "bite-size" "blackcurrant" "blackness"
          "blameless" "blare" "blaspheme" "blindly" "blindness" "blissful" "bloat"
          "blotch" "blubber" "bluffy" "bluish" "bluster" "boastful" "bookcase"
          "bookkeeper" "bookworm" "borough" "braid" "brainless" "brainpower" "brand-new"
          "brash" "brilliance" "brother-in-law" "brute" "built-in" "bystander" "calmness"
          "calumny" "candour" "capacious" "captivate" "carbohydrate" "carbon" "carious"
          "carnivorous" "cast iron" "catchy" "cater" "cattish" "ceasless" "ceramic"
          "chafe" "chagrin" "chancy" "changeable" "changeless" "chastise" "chastisement"
          "chatterbox" "cheapen" "cheerless" "chestnut" "childbirth" "churchyard"
          "circular" "circumscribe" "circumvent" "civic" "clack" "clairvoyant" "cleave"
          "cleaver" "cleft" "clemency" "clement" "clergy" "clergyman" "cleverness"
          "climax" "clinch" "cloakroom" "closeness" "clotted" "cloudless" "clout"
          "cluster" "clutter" "cockpit" "coefficient" "cogitate" "cognate" "cognizance"
          "cognizance" "cohabit" "cohere" "cohort" "colander" "coldness" "collateral"
          "collective" "collide" "comely" "commandment" "commemorate" "commiserate"
          "compulsion" "concealment" "concede" "concourse" "concourse" "concur" "concur"
          "concur" "concurrence" "condiment" "condole" "conductive" "congest"
          "congestion" "congruous" "connate" "consecution" "conserve" "cosign"
          "constrict" "contagion" "contaminate" "contemn" "convene" "cookery" "corpulent"
          "cordial" "cornflower" "correspondence" "correlation" "credulity" "crimpy"
          "culinary" "dashboard" "daughter-in-law" "daunt" "dauntless" "Daydream"
          "daydream" "debark" "debatable" "debauch" "decimal" "decisive" "decoction"
          "decompose" "decree" "deep-rooted" "deface" "deference" "deficit" "deliquency"
          "demeanour" "demolition" "demote" "denote" "denounce" "density" "denunciation"
          "depart" "deplete" "deprave" "derelict" "detestable" "dietary" "diffuse"
          "digestive" "dignify" "dilapidation" "dilution" "diminution" "dimness" "dingy"
          "dipping" "directness" "dirtiness" "disability" "disarray" "disavow"
          "discernible" "disciplinary" "discourse" "discourteous" "discrepancy"
          "disembody" "disembroil" "dish-water" "disingenuous" "disinherit" "disloyal"
          "dismay" "disobey" "disparage" "disparity" "disprove" "disreputable"
          "dissemble" "dissimilar" "distasteful" "distraction" "diverse" "diverse"
          "divert" "divinity" "doable" "dog-collar" "domain" "doormant" "draught"
          "drawback" "earlap" "earthy" "eatable" "easygoing" "edacity" "edacious"
          "efforless" "ejection" "elemental" "emasculate" "embitter" "embodiment"
          "eminent" "emulate" "encase" "endorsement" "enfold" "enigma" "ensign"
          "enthrone" "equability" "equality" "equity" "erratic" "espionage" "estrange"
          "eventful" "evergreen" "evoke" "exasperate" "excavate" "exclusion" "excrete"
          "excruciation" "exemplar" "facile" "facultative" "fairness" "fairytale"
          "faithless" "falsify" "far-fetched" "father-in-law" "faultless" "fearsome"
          "feasible" "feint" "fellowship" "fervid" "fetid" "feudal" "fickle" "finesse"
          "fire-extinguisher" "firstly" "fitful" "fixedly" "fixture" "fizzy" "flabby"
          "flamboyant" "flank" "flatly" "fleck" "floral" "fluency" "flurry" "foggy"
          "foetus" "follower" "fondle" "fondness" "footwear" "forceful" "forceless"
          "forearm" "foresee" "foresight" "foretaste" "forgetful" "forum" "foster-child"
          "frigility" "fraudulent" "freelance" "frivolous" "frizz" "frontage" "frugality"
          "fruitless" "gabble" "gaily" "gainful" "gateway" "gaudy" "glare" "gleeful"
          "glossary" "glutton" "gluttony" "goblet" "godless" "goggle" "gorge" "gossipy"
          "governess" "graceless" "grate" "groin" "groundless" "growl" "gruel"
          "hailstone" "hairdresser" "half-baked" "half-brother" "half-moon" "half-time"
          "half-year" "halve" "hamper" "handgrip" "handicraft" "hangnail" "hard-boiled"
          "harden" "hardily" "hardness" "hardship" "headlight" "headphone" "heady"
          "heartily" "heedless" "heiress" "heirloom" "hermic" "hibernate" "hiccup"
          "hindrance" "hindsight" "hireling" "hobble" "housemaid" "hugely" "humankind"
          "humid" "husky" "hysteria" "icily" "illegible" "illegimate" "illicit"
          "illusive" "imbue" "immaculate" "immensely" "immerse" "impassable" "imperfect"
          "impetuous" "impish" "implacable" "impregnable" "imprint" "inaction"
          "inadequacy" "inane" "inapptitude" "inattentive" "indelicate" "indicator"
          "indigenous" "indiscreet" "indulgence" "inedible" "inept" "inexpedient"
          "inexperienced" "inexplicable" "inextricable" "infertile" "inflammable"
          "infractructure" "inhibit" "inhospitable" "inset" "intrusive" "invoke"
          "invulnerable" "irradiate" "irritable" "jaguar" "jangle" "jargon" "jaunty"
          "jobless" "joggle" "joyful" "juxtapose" "juxtaposition" "keyhole" "keynote"
          "knead" "lacerate" "lagoon" "lanentable" "lamppost" "lamp-shade" "lance"
          "landfall" "landmark" "languid" "lardy" "lascivious" "lassitude" "latent"
          "lavishness" "laxative" "leap year" "leaseholder" "legalize" "leggy" "lenient"
          "levity" "libellous" "likeable" "likelihood" "limelight" "limelight"
          "limitless" "lineal" "linear" "lioness" "liquidate" "listless" "literacy"
          "lithe" "livestock" "locus" "lollipop" "lozenge" "lucent" "lucidity" "lackless"
          "lucrative" "lumpy" "magnanimity" "majestic" "malicious" "marshy" "mediocre"
          "mercenary" "mermaid" "merriment" "mesmerize" "middle-aged" "midland"
          "midsummer" "midwife" "migration" "migrant" "mildness" "mileage" "milestone"
          "minder" "mindless" "minefield" "misfire" "misfortune" "mishandle" "mislead"
          "misspell" "mussel" "muteness" "mutinous" "nameless" "namely" "namesake"
          "naval" "navel" "navigate" "nebulous" "needful" "negate" "negation"
          "negligence" "negligent" "neighbourly" "nestle" "neutrality" "nightfall"
          "nightingale" "noiseless" "nomad" "nomad" "nonchalance" "nonpareil" "nosebleed"
          "notability" "notary" "notoriety" "nudge" "nurture" "nutmeg" "nutrient"
          "obdurate" "obedience" "objectless" "obliterate" "obscenity" "observance"
          "obsess" "obstinate" "obtrusive" "occult" "occupant" "occurrence" "octopus"
          "offender" "offset" "old-fashioned" "ominous" "omnipotent" "omnipresent"
          "oncoming" "onefold" "onerous" "open-air" "open-minded" "opinionated"
          "opinionated" "oppression" "ostensible" "outbalance" "outbid" "outbreak"
          "outburst" "outlay" "outmatch" "outnumber" "outright" "outset" "outskirts"
          "overjoyed" "overlap" "overlive" "oversleep" "overweight" "oxide" "pacify"
          "palette" "pallor" "palmful" "palpable" "palpate" "palsy" "pancreas" "parable"
          "parity" "parquet" "parting" "passable" "paternal" "patronage" "pavilion"
          "peachy" "pectoral" "peculiarity" "peephole" "peerless" "peevish" "pendulous"
          "penitence" "penitent" "penniless" "pensioner" "peppery" "percussive"
          "peremptory" "perennial" "perfunctory" "periodic" "permissive" "perpetrate"
          "perpetrator" "persevere" "personify" "perspiration" "perspire" "peruse"
          "pervasive" "pester" "pestilent" "petulant" "picturesque" "pictorial"
          "piercing" "piety" "pillar" "pillowcase" "pilotage" "pine cone" "pinprick"
          "placidity" "plash" "platitude" "plenitude" "pliant" "plumb" "plume" "plump"
          "poignant" "polity" "pollen" "pomegranate" "pomposity" "ponderous" "populate"
          "portent" "portly" "portray" "possible" "postal" "posture" "potency" "prawn"
          "precursor" "predestination" "predestine" "premonition" "preparation" "prepay"
          "prepense" "prevalent" "prevention" "prickly" "primeval" "proclamation"
          "prodigal" "prodigious" "profane" "profanity" "profess" "progeny" "prohibition"
          "projection" "propel" "prosperity" "prosperity" "provisional" "prowess" "prowl"
          "prudent" "puncture" "purge" "purify" "pushful" "putrefy" "putrescent"
          "puzzlement" "quake" "quaking" "quandary" "quarrelsome" "quasi" "quell"
          "quench" "rabid" "radiant" "rancid" "rancorous" "rapacity" "rarity" "ravenous"
          "rebellion" "rebuff" "recant" "recast" "recede" "reseptive" "recipient"
          "reciprocal" "recite" "reckon" "rectangle" "rectification" "recuperate"
          "recurrent" "redden" "redeem" "redress" "referee" "reform" "refrigerate"
          "refuse" "regretful" "regrettable" "relapse" "reliance" "reliant" "relish"
          "remembrance" "remiss" "renaissance" "renewal" "repellent" "replica" "repose"
          "reprehensible" "reprove" "repute" "resemble" "resentful" "resedue" "reverence"
          "revert" "revoke" "revolve" "revulsion" "rigour" "riotous" "rotund"
          "roundabout" "rucksack" "rudimentary" "ruinous"
          ))
  (defun build-fuzzy-reg (str &optional left-bndry right-bndry constituent)
    ;; TODO properly escape * > \\ etc. in STR
    (unless left-bndry (setq left-bndry "\\_<"))
    (unless right-bndry (setq right-bndry "\\_>"))
    (unless constituent (setq constituent "\\(\\w\\|\\s_\\)"))
    (concat left-bndry
            (mapconcat (lambda (c)
                         (concat (string c)
                                 constituent
                                 "*"))
                       str "")
            right-bndry))
  (defun enable-company-english ()
    (interactive)
    (set (make-local-variable 'company-backends)
         '((company-english :separate company-dabbrev :separate company-file))))
  (defun disable-company-english ()
    (interactive)
    (kill-local-variable 'company-backends))
  (defun company-english (command &optional arg &rest ignored)
    "`company-mode' backend for english"
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-english))
      (prefix      (when (looking-back "\\_<.+")
                     (match-string 0)))
      (candidates  (let ((res nil) (reg (build-fuzzy-reg arg)))
                     (dolist (w english-words)
                       (when (string-match-p reg w)
                         (push w res)))
                     (nreverse res)))
      ;; (annotation  ...)
      (meta        (format "This value is named %s" arg)))))


;; =================
;; Backend Example
;; (defun company-simple-wordlist-backend (command &optional arg &rest ignored)
;;   (interactive (list 'interactive))
;;   (case command
;;     (interactive (company-begin-backend 'company-simple-wordlist-backend))
;;     (prefix      (when (looking-back "\\_<.+")
;;                    (match-string 0)))
;;     (candidates  (let ((res nil))
;;                    (dolist (w (cdr (assq major-mode wordlist)))
;;                      (when (string-prefix-p arg w)
;;                        (push w res)))
;;                    (nreverse res)))
;;     ;; (annotation  ...)
;;     (meta        (format "This value is named %s" arg))))
;; =================




;; Hippie expand
;; TODO
;;  priority = currentBuf > otherBufOfSameType > Dict > otherBufs
;;  evaluation function
;;  completion from file-type specific dictionary (with different keybinding)

;; (use-package hippie-exp
;;   :commands (hippie-expand my-he/file-comp-cmd)

;;   :config
;;   (setq hippie-expand-verbose nil)

;;   (setq hippie-expand-try-functions-list
;;         '(
;;           my-he/try-expand-hist
;;           my-he/try-expand-fuzzy
;;           ;; my-he/try-expand-test
;;           ))

;;   (defun my-he/ordered-lset-diff (l1 l2 same?)
;;     "Compute set difference L1 - L2 (using SAME? for equality test).
;; Returns newly allocated list."
;;     (let ((res nil))
;;       (dolist (a l1)
;;         (let ((in-l2 nil)
;;               (l l2))
;;           (while l
;;             (if (funcall same? a (car l))
;;                 (setq in-l2 t
;;                       l nil)
;;               (setq l (cdr l))))
;;           (unless in-l2
;;             (push a res))))
;;       (nreverse res)))

;;   (defun my-he/remove (elt lis same?)
;;     "Delete ELT from LIS using SAME? for equality test."
;;     (my-he/ordered-lset-diff lis (list elt) same?))

;;   (defun my-he/remove-dups (lis same?)
;;     "Remove duplicates in LIS using SAME? for equality test."
;;     (let ((res nil) (l lis))
;;       (while l
;;         (push (car l) res)
;;         (setq l (my-he/remove (car l) (cdr l) same?)))
;;       (nreverse res)))

;;   ;; Hist completion with fuzzy matching

;;   (defun my-he/try-expand-hist (has-init)
;;     (unless has-init
;;       ;; init region
;;       (he-init-string (he-lisp-symbol-beg) (point))
;;       ;; add original text to he-tried-table
;;       (add-to-list 'he-tried-table he-search-string)
;;       ;; set he-expand-list to candidate list
;;       ;; ensure no completion duplicates
;;       (let ((l (my-he/matching-hists he-search-string)))
;;         (setq he-expand-list
;;               (my-he/ordered-lset-diff l he-tried-table 'string=))))

;;     ;; do completion or reset, and return nil iff there's no more candidates
;;     (cond
;;      ((null he-expand-list)
;;       ;; reset region if has-init
;;       (when has-init (he-reset-string))
;;       nil)
;;      (t
;;       ;; completion with first candidate
;;       (let ((cand (pop he-expand-list)))
;;         (he-substitute-string cand)
;;         (my-he/add-hist cand))
;;       t)))

;;   (defvar my-he/hist nil)
;;   (defun my-he/add-hist (str)
;;     (setq my-he/hist (my-he/remove str my-he/hist 'string=))
;;     (when (< 100 (length my-he/hist)) (setcdr (nthcdr 100 my-he/hist) nil))
;;     (push str my-he/hist))
;;   (defun my-he/matching-hists (str)
;;     (let ((regexp (my-he/create-fuzzy-regexp str))
;;           (res nil))
;;       (dolist (hist my-he/hist)
;;         (when (string-match regexp hist) (push hist res)))
;;       (nreverse res)))

;;   ;; Fuzzy completion

;;   (defun my-he/try-expand-fuzzy (has-init)
;;     (unless has-init
;;       ;; init region
;;       (he-init-string (he-lisp-symbol-beg) (point))
;;       ;; add original text to he-tried-table
;;       (add-to-list 'he-tried-table he-search-string)
;;       ;; set he-expand-list to candidate list
;;       ;; ensure no completion duplicates
;;       (let ((l (my-he/collect-regmatch (my-he/create-fuzzy-regexp he-search-string) t)))
;;         (setq he-expand-list
;;               (my-he/ordered-lset-diff l he-tried-table 'string=))))

;;     ;; do completion or reset, and return nil iff there's no more candidates
;;     (cond
;;      ((null he-expand-list)
;;       ;; reset region if has-init
;;       (when has-init (he-reset-string))
;;       nil)
;;      (t
;;       ;; completion with first candidate
;;       (let ((cand (pop he-expand-list)))
;;         (he-substitute-string cand)
;;         (my-he/add-hist cand))
;;       t)))

;;   (defun my-he/create-fuzzy-regexp (str &optional left-bndry right-bndry constituent)
;;     ;; TODO properly escape * > \\ etc. in STR
;;     (unless left-bndry (setq left-bndry "\\_<"))
;;     (unless right-bndry (setq right-bndry "\\_>"))
;;     (unless constituent (setq constituent "\\(\\w\\|\\s_\\)"))

;;     (concat left-bndry
;;             (mapconcat (lambda (c)
;;                          (concat (string c)
;;                                  constituent
;;                                  "*"))
;;                        str "")
;;             right-bndry))

;;   (defun my-he/collect-regmatch (re &optional all-buffers)
;;     (let ((collection nil)
;;           (buffers (if all-buffers
;;                        (append (remove (current-buffer) (buffer-list))
;;                                (list (current-buffer)))
;;                      (list (current-buffer)))))
;;       (save-excursion
;;         (dolist (b buffers)
;;           (with-current-buffer b
;;             (goto-char (point-min))
;;             (while (search-forward-regexp re nil t)
;;               ;; or use (thing-at-point 'symbol t) ?
;;               (push (buffer-substring-no-properties (match-beginning 0)
;;                                                     (match-end 0))
;;                     collection)))))
;;       (my-he/remove-dups collection 'string=)))

;;   ;; Filename completion

;;   (fset 'my-he/file-comp-cmd (make-hippie-expand-function '(try-complete-file-name-partially
;;                                                             try-complete-file-name)))
;;   (fset 'my-he/yas (make-hippie-expand-function '(yas-hippie-try-expand)))
;;   )

;; Hippie documentation {{{

;; (he-init-string beg end)
;;   set the region from BEG to END to be expanded. also sets he-search-string

;; he-search-string  (variable)
;;   the original text in the expanded region (set when he-init-string runs)

;; (he-string-member str lst trans-case)
;;   same as (member str lst) unless trans-case is non-nil

;; (he-reset-string)
;;   reset the expanded region (to its initial contents)

;; (he-substitute-string str)
;;   substitute the expanded region with str

;; he-tried-table  (variable)
;;   list containing all tried expansions so far

;; The argument OLD of try-*** functions is nil in the first call or t otherwise


;; Example try-expand function

;; (defun my-he/try-expand-test (has-init)
;;   (unless has-init
;;     ;; init region
;;     (he-init-string (he-lisp-symbol-beg) (point))
;;     ;; add original text to he-tried-table
;;     (add-to-list 'he-tried-table he-search-string)
;;     ;; set he-expand-list to candidate list
;;     ;; ensure no duplicates in he-expand-list
;;     ;; ensure no strings from he-tried-table in he-expand-list
;;     (let ((l '("apple" "banana" "ant" "basketball")))
;;       (setq l (delete-dups (copy-sequence l)))
;;       (dolist (x he-tried-table) (delete x l))
;;       (setq he-expand-list l)))

;;   ;; do completion or reset, and return nil iff there's no more candidates
;;   (cond
;;    ((null he-expand-list)
;;     ;; reset region if has-init
;;     (when has-init (he-reset-string))
;;     nil)
;;    (t
;;     ;; completion with the first candidate
;;     (let ((cand (pop he-expand-list)))
;;       (he-substitute-string cand))
;;     t)))

;; }}}

(provide 'company-keywords-plus)
;;; company-keywords-plus.el ends here

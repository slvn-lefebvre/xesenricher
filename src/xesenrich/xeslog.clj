(ns xesenrich.xeslog)

(defn write-timestamp
  [file timestamp]
  (spit file  (str "<date key=\"time:timestamp\" value=\"" timestamp "\"/>\n") :append true))


(defn write-executors [file executors]
  (if (not= nil executors)
    (doseq [e executors]
    (spit file (str "<string key=\"org:resource\" value=\""
                    e
                    "\"/>\n") :append true))))

(defn write-component-id
  [file id]
  (spit file (str  "<string key=\"concept:name\" value=\""
                   id
                   "\"/>\n") :append true))


;; type for Person record yields "xesenrich.bpmn.Person"
(defn write-state
  [file lifecycle state]
  (spit file
        (str "<string key=\""
             lifecycle
             ":transition\" value=\""
             state
             "\"/>\n") :append true))


;; main logging function
(defn add-business-log
  [file clock lifecycle entityid state executors]
  (spit file "<event>\n" :append true)
  (write-timestamp file clock)
  (write-component-id file entityid )
  (write-state file lifecycle state)
  (write-executors file executors)
  (spit file "</event>\n" :append true))

  

(def EXTENSION ".xes")

(defn create-instance
  [id]
  (spit (str id EXTENSION)
        (str
         "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
        "<log xes.version=\"1.0\" xmlns=\"http://www.xes-standard.org\" xes.creator=\"xesenrich\">\n"
	"<extension name=\"Concept\" prefix=\"concept\" uri=\"http://www.xes-standard.org/concept.xesext\"/>\n"
        "<extension name=\"Time\" prefix=\"time\" uri=\"http://www.xes-standard.org/time.xesext\"/>\n"
        "<extension name=\"Organizational\" prefix=\"org\" uri=\"http://www.xes-standard.org/org.xesext\"/>\n"
	"<global scope=\"trace\">\n"
	"<string key=\"concept:name\" value=\"name\"/>\n"
	"</global>\n"
	"<global scope=\"event\">\n"
	"<string key=\"concept:name\" value=\"name\"/>\n"
	"<string key=\"org:resource\" value=\"resource\"/>\n"
	"<string key=\"Activity\" value=\"string\"/>\n"
	"<string key=\"Resource\" value=\"string\"/>\n"
	"<string key=\"Costs\" value=\"string\"/>\n"
	"</global>\n"
	"<classifier name=\"Activity\" keys=\"Activity\"/>\n"
	"<classifier name=\"activity classifier\" keys=\"Activity\"/>\n"
	"<trace>\n"
	"<string key=\"concept:name\" value=\""
	 id "\" />\n"
         "<string key=\"creator\" value=\"XES enricher\"/>\n")
        ))

(defn close-instance
  [file]
  (spit file
        (str "</trace>\n"
             "</log>\n") :append true))




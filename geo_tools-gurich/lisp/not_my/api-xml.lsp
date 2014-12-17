;;;************************************************************************
;;; api-xml.lsp
;;; Prepared by: J. Szewczak
;;; Date: 4 January 2004
;;; Purpose: To provide an API for interfacing with XML files.
;;; Copyright (c) 2004 - AMSEC LLC - All rights reserved
;;;************************************************************************
;;; Version 2004.01.04
;;;************************************************************************

;;;***********************************************************************
;;; MODULE: api-error
;;; DESCRIPTION: wraps a function to trap Active-X errors - if error is found
;;; DESCRIPTION: function returns nil.
;;; ARGS: function to check, list of arguments, boolean (return error message?) T or nil
;;; EXAMPLE: (api-error '/ (list 50 0) T) displays "VLISP Error: divide by zero" & returns 'nil'
;;;***********************************************************************

(defun api-error (func lst bool / trap)
  (cond
    ( (vl-catch-all-error-p
        (setq trap (vl-catch-all-apply func lst))
      )
      (if bool (princ (strcat "\nVLISP XML Error: " (vl-catch-all-error-message trap))))
      (setq trap nil)
    )
  )
  trap
)


;;;************************************************************************
;;; MODULE: XML-Get-Document
;;; DESCRIPTION: queries an XML file for the DOM Active-X object
;;; ARGS: XML file (string); a variable to store the DOM object
;;; EXAMPLE:(XML-Get-Document projfile 'XMLDoc) returns vla-object
;;;************************************************************************

(defun XML-Get-Document (file XMLDoc)
  (if (findfile file)
    (progn
      (set XMLDoc (vlax-create-object "MSXML2.DOMDocument.3.0")) ;;create XML-DOM pipeline
      (vlax-put-property (eval XMLDoc) "async" :vlax-false)
      (cond
        ( (api-error 'vlax-invoke-method (list (eval XMLDoc) "Load" file) T) ;; Load Project File into XML-DOM pipeline
          (eval XMLDoc)
        )
      )
    )
    (alert "\nXML Document could not be found.")
  )
)

;;;************************************************************************
;;; MODULE: XML-Get-XMLObject
;;; DESCRIPTION: this gets the top-level parent node object in a given XML Document
;;; ARGS: filename (string)
;;; EXAMPLE: (XML-Get-XMLObject filename) returns VLA-OBJECT
;;;************************************************************************

(defun XML-Get-XMLObject (file / docObj xmlTop xmlVer object result)
  (if (findfile file)
    (progn
      (setq docObj (XML-Get-Document file 'docObj))
      (setq xmlTop (vlax-get-property docObj "childNodes")) ;; Get the Top Level of the XML
      (setq xmlVer (vlax-invoke-method xmlTop "nextNode"))  ;; Gets the XML version element
      (if(not(setq object (vlax-invoke-method xmlTop "nextNode")))(setq object xmlVer));; Gets the Parent element
      (if (=(vlax-get-property object "nodeName")"#comment")
	(setq result (vlax-invoke-method xmlTop "nextNode"))
	(setq result object))
      )
    )
  result
  )

;;;************************************************************************
;;; MODULE: XML-Get-ElementKey
;;; DESCRIPTION: returns the requested tags text.
;;; ARGS: Parent - the parent collection object, tag name - must be unique tag name
;;; EXAMPLE: (XML-Get-ElementKey laydef "Name") returns "ANNOTATION"
;;;************************************************************************

(defun XML-Get-ElementKey (parent tag / el desc)
  (if (vlax-method-applicable-p parent 'getElementsByTagName)
    (progn
      (setq el (vlax-invoke-method parent 'getElementsByTagName tag))
      (if (> (vlax-get-property el 'Length) 0)
        (setq desc (vlax-get-property (vlax-invoke-method el 'nextNode) 'text))
      )
      (vlax-invoke-method el 'reset)
    )
    (princ "\nXML Object could not be searched.")
  )
  (if desc desc nil)
)

;;;************************************************************************
;;; MODULE: XML-Get-ElementKey-Object
;;; DESCRIPTION: returns the requested tags object.
;;; ARGS: Parent - the parent collection object, tag name - must be unique tag name
;;; EXAMPLE: (XML-Get-ElementKey-Object laydef "Name") returns VLA-OBJECT
;;;************************************************************************

(defun XML-Get-ElementKey-Object (parent tag / el desc)
  (if (vlax-method-applicable-p parent 'getElementsByTagName)
    (progn
      (setq el (vlax-invoke-method parent 'getElementsByTagName tag))
      (if (> (vlax-get-property el 'Length) 0)
        (setq desc (vlax-invoke-method el 'nextNode))
      )
      (vlax-invoke-method el 'reset)
    )
    (princ "\nXML Object could not be searched.")
  )
  (if desc desc nil)
)

;;;************************************************************************
;;; MODULE: XML-Get-Parent
;;; DESCRIPTION: Gets a top level object from the XML object with the given name
;;; ARGS: a valid XML object, the name of the parent level
;;; EXAMPLE: (XML-Get-Parent oXML "Leaders") returns VLA-OBJECT
;;;************************************************************************

(defun XML-Get-Parent (oXML name)
  (vlax-invoke-method
    (vlax-invoke-method oXML 'GetElementsByTagName name)
   'peekNode
  )
)

;;;************************************************************************
;;; MODULE: XML-Get-Children
;;; DESCRIPTION: gets the child object from a parent level
;;; ARGS: XML object, Parent name
;;; EXAMPLE: none
;;;************************************************************************

(defun XML-Get-Children (oXML parentName / return)
  (cond
    ( (/= parentName nil)
      (if (vlax-invoke-method (XML-Get-Parent oXML parentName) 'hasChildNodes)
        (setq return (vlax-get-property (XML-Get-Parent oXML parentName) 'childNodes))
      )
    )
    ( T (if (vlax-invoke-method oXML 'hasChildNodes) (setq return (vlax-get-property oXML 'childNodes))))
  )
  return
)

;;;************************************************************************
;;; MODULE: XML-Get-ChildList
;;; DESCRIPTION: returns a list of all child objects under a parent XML Object
;;; ARGS: XML object
;;; EXAMPLE: (XML-Get-ChildList objXML) returns a list of VLA-Objects
;;;************************************************************************

(defun XML-Get-ChildList (oXML / collection child lst)
  (cond
    ( (vlax-invoke-method oXML 'hasChildNodes)
     (setq collection (XML-Get-Children oXML nil))
     (while (setq child (vlax-invoke-method collection 'nextNode))
       (setq lst (if lst (cons child lst) (list child)))
       )
     (reverse lst)
     )
    (princ "\nObject has no children")
  )
)

;;;************************************************************************
;;; MODULE: XML-Get-Child
;;; DESCRIPTION: gets a specific child name under the parent level with given name
;;; ARGS: XML object, parent level, child name
;;; EXAMPLE: (setq oPointers (XML-Get-Child oXML "Leaders" "Pointers")) returns VLA-Object
;;; EXAMPLE: (XML-Get-Child oPointers nil "ArrowSize") used you only want to go one deep...
;;;************************************************************************

(defun XML-Get-Child (oXML parentName childName / child target)
  (cond
    ( (/= parentName nil) (setq child (XML-get-Children oXML parentName)))
    ( T (setq child (vlax-get-property oXML 'childNodes)))
  )
  (setq target (api-error 'vlax-invoke-method (list child 'nextNode) T))
  (while
    (and
      target
      (/= (vlax-get-property target 'tagName) childName)
    )
    (setq target (api-error 'vlax-invoke-method (list child 'nextNode) T))
  )
  target
)

;;;************************************************************************
;;; MODULE: XML-Get-Child-ByAttribute
;;; DESCRIPTION: gets a specific child name under the parent level with given attribute and attribute value
;;; ARGS: XML object, parent level, attribute to search by, attribute value to match
;;; EXAMPLE: (XML-Get-Child-ByAttribute oLayers "LayerDefinitions" "Name" "ANNOTATION") returns VLA-OBJECT
;;; EXAMPLE: (XML-Get-Child-ByAttribute oLayerDefs nil "Name" "ANNOTATION") used when you only want to go one deep...
;;;************************************************************************

(defun XML-Get-Child-ByAttribute (oXML parentName attrib attribValue / parent rtn)
  (if parentName
    (setq parent (XML-Get-Child oXML nil parentName))
    (setq parent oXML)
  )
  (foreach itm (XML-Get-ChildList parent)
    (if (= (XML-Get-Attribute itm attrib "") attribValue)
      (setq rtn itm)
    )
  )
  rtn
)

;;;************************************************************************
;;; MODULE: XML-Get-Child-Value
;;; DESCRIPTION: Retrieves the value from the 'Text property of a child element
;;; ARGS: XML object, parent Name, Child Name
;;; EXAMPLE: (XML-Get-Child-Value oXML "TaskInfo" "FSCM") returns "4T323"
;;;************************************************************************

(defun XML-Get-Child-Value (oXML parentName childName)
  (if (XML-Get-Child oXML parentName childName)
    (vlax-get-property (XML-Get-Child oXML parentName childName) 'text)
    nil
  )
)

;;;************************************************************************
;;; MODULE: XML-Put-Child
;;; DESCRIPTION: updates the text in a given child node
;;; ARGS: XML object, parent node name, child node to change, value to change to
;;; EXAMPLE: (XML-Put-Child oXML "DrawingSetup" "DrawingMode" "MicroScale")
;;;************************************************************************

(defun XML-Put-Child (oXML parentName childName valu / child return)
  (if
    (and
      valu
      (setq valu (vl-princ-to-string valu))
      (setq child (XML-Get-Child oXML parentName childName))
    )
    (api-error 'vlax-put-property (list child 'text valu) T)
  )
)

;;;************************************************************************
;;; MODULE: XML-Remove-Child
;;; DESCRIPTION: removes the specified child object node
;;; ARGS: XML node object
;;; EXAMPLE: (XML-Remove-Child oXML)
;;;************************************************************************

(defun XML-Remove-Child (rmvChild / parent)
  (setq parent (vlax-get-property rmvChild 'parentNode))
  (api-error 'vlax-invoke-method (list parent 'removeChild rmvChild) T)
)

;;;************************************************************************
;;; MODULE: XML-Add-Child
;;; DESCRIPTION: adds a Child element to the given parent object
;;; ARGS: Parent Level VLA-Object, name of new child
;;; EXAMPLE: (XML-Add-Child oXML "LayerKey") returns the newly created child node object
;;;************************************************************************

(defun XML-Add-Child (parent name / xmlDoc newElement return)
  (setq xmlDoc (vlax-get-property parent 'ownerDocument))
  (if (not parent) (setq parent xmlDoc))
  (if (setq newElement (api-error 'vlax-invoke-method (list xmlDoc 'createElement name) T))
    (setq return (api-error 'vlax-invoke-method (list parent 'appendChild newElement) T))
  )
  return
)

;;;************************************************************************
;;; MODULE: XML-Get-Attribute
;;; DESCRIPTION: returns an XML object's named attribute value
;;; ARGS: XML object, attribute name (string), a default value to return if there is no value found in the XML object
;;; EXAMPLE: (XML-Get-Attribute oXML "Name" "Default") might return "ANNOTATION"
;;;************************************************************************

(defun XML-Get-Attribute (oXML name default / att return)
  (if (setq att (api-error 'vlax-invoke-method (list oXML 'getAttributeNode name) T))
    (if att
      ;; due to the fact that the Text property strips trailing and leading white characters
      ;; when using the 'get' method.  the Value property has been used instead.
      (vlax-variant-value (vlax-get-Property att "Value"))
      default
    )
  )
)

;;;************************************************************************
;;; MODULE: XML-Get-Attribute-List
;;; DESCRIPTION: returns a list of strings corresponding to the names of the XML object's attributes
;;; ARGS: XML object
;;; EXAMPLE: (XML-Get-Attribute-List oXML) might return ("Name" "Color" "LineType" "LineWeight" "Plottable" "Comment")
;;;************************************************************************

(defun XML-Get-Attribute-List (oXML / lst attCollection count)
  (if (setq attCollection (vlax-get-property oXML 'attributes))
    (progn
      (setq count 0)
      (while (< count (vlax-get-property attCollection 'length))
        (setq lst (append lst (list (vlax-get-property (vlax-get-property attCollection 'item count) 'Name)))
              count (1+ count)
        )
      )
    )
  )
  lst
)

;;;************************************************************************
;;; MODULE: XML-Put-Attribute
;;; DESCRIPTION: function to put an XML attribute value -- will add the attribute if not already there
;;; ARGS: XML object, attribute name (string), a value to change the attribute to
;;; EXAMPLE: (XML-Put-Attribute oXML "Name" "Default") might return vla-object
;;;************************************************************************

(defun XML-Put-Attribute (oXML name valu / att return)
  (cond
    ( valu (setq valu (vl-princ-to-string valu)))
    ( (not valu) (setq valu ""))
  )
  (if (not (XML-Get-Attribute oXML name nil))
    (XML-Add-Attribute oXML name "")
  )
  (api-error 'vlax-invoke-method (list oXML 'setAttribute name valu) T)
)

;;;************************************************************************
;;; MODULE: XML-Add-Attribute
;;; DESCRIPTION: adds an attribute to the Parent Object with given name, and the optional value
;;; ARGS: Parent Level VLA-Object, name of new attribute, value (optional)
;;; EXAMPLE: (XML-Add-Attribute oXML "Name" "Default") returns the newly created attribute XML object.
;;;************************************************************************

(defun XML-Add-Attribute (parent name valu / xmlDoc newAttribute newAtt)
  (setq xmlDoc (vlax-get-property parent 'ownerDocument))
  (if (setq newAttribute (api-error 'vlax-invoke-method (list xmlDoc 'createAttribute name) T))
    (progn
      (setq attNodeMap (vlax-get-property parent 'attributes)
            newAtt (api-error 'vlax-invoke-method (list attNodeMap 'setNamedItem newAttribute) T)
      )
      (if valu (vlax-put-property newAtt 'Text valu))
    )
  )
  newAtt
)

;;;************************************************************************
;;; MODULE: XML-Get-Value
;;; DESCRIPTION: Retrieves the value from the 'Text property of the supplied XML Element object
;;; ARGS: XML object
;;; EXAMPLE: (XML-Get-Value oXML) returns "4T323"
;;;************************************************************************

(defun XML-Get-Value (oXML)
  (api-error 'vlax-get-property (list oXML 'text) T)
)

;;;************************************************************************
;;; MODULE: XML-Put-Value
;;; DESCRIPTION: Puts a text value into the 'Text property of the supplied XML Element object
;;; ARGS: XML object, text string
;;; EXAMPLE: (XML-Put-Value oXML "4T323") returns T if successful nil if not.
;;;************************************************************************

(defun XML-Put-Value (oXML str)
  (api-error 'vlax-put-property (list oXML 'text str) T)
  (if (= (XML-Get-Value oXML) str)
    T
    nil
  )
)

;;;************************************************************************
;;; MODULE: XML-SaveAs
;;; DESCRIPTION: Writes the parsed XML object out to the given file
;;; ARGS: XML document object or XML element object, fully qualified filename to save to
;;; EXAMPLE: (XML-SaveAs oXML "C:\\projects\\npy structure.swp")
;;;************************************************************************

(defun XML-SaveAs (oXML file)
  (cond
    ( (= (vlax-get-property oXML 'nodeTypeString) "element")
      (api-error 'vlax-invoke-method (list (vlax-get-property oXML 'ownerDocument) 'save file) T)
    )
    ( T (api-error 'vlax-invoke-method (list oXML 'save file) T))
  )
)

;;;************************************************************************
;;; MODULE: XML-Save
;;; DESCRIPTION: Writes the parsed XML object out to its parent file
;;; ARGS: XML document object or XML element object
;;; EXAMPLE: (XML-Save oXML)
;;;************************************************************************

(defun XML-Save (oXML / doc file)
  (cond
    ( (= (vlax-get-property oXML 'nodeTypeString) "element")
      (setq doc (vlax-get-property oXML 'ownerDocument))
    )
    ( T (setq doc oXML))
  )
  (setq file (vl-string-subst "\\" "/" (vl-string-left-trim "file:///" (vlax-get-property doc 'url))))
  (api-error 'vlax-invoke-method (list doc 'save file) T)
)



(defun XML-Remove-Attribute (oXML name / att)
  (not(api-error 'vlax-invoke-method (list oXML 'removeAttribute name) T))
  )




(defun XML-Get-Child-ByTeg-and-Attribute (oXML parentName attrib attribValue Teg / parent rtn)
  (if parentName
    (setq parent (XML-Get-Child oXML nil parentName))
    (setq parent oXML)
    )
  (foreach itm (XML-Get-ChildList parent)
    (if (and (= (XML-Get-Attribute itm attrib "") attribValue) (=(vlax-get-property itm 'tagName) Teg))
      (setq rtn itm)
      )
    )
  rtn
  )

;;;(defun XML-Get-ChildList (oXML / collection child lst)
;;;  (cond
;;;    ( (vlax-invoke-method oXML 'hasChildNodes)
;;;     (setq collection (XML-Get-Children oXML nil))
;;;     (while (setq child (vlax-invoke-method collection 'nextNode))
;;;       (setq lst (if lst (cons child lst) (list child)))
;;;       )
;;;     (reverse lst)
;;;     )
;;;    (princ "\nObject has no children")
;;;  )
;;;)
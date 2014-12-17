;;;************************************************************************
;;;(princ)
;;;************************************************************************
;;;************************************************************************
;;;************************************************************************
;;;************************************************************************


<?xml version="1.0"?>
<Settings>
  <Layers>
    <Layer Name="Walls" Color="" LineType="continuous"/>
    <Layer Name="Furniture" Color="3" LineType="HIDDEN"/>
  </Layers>
  <DrawingVars>
    <RegenAuto>1</RegenAuto>
    <EdgeMode>0</EdgeMode>
    <Osmode>383</Osmode>
  </DrawingVars> 
</Settings>

;;; Load the VisualLISP stuff
(vl-load-com)

;;; Store an Active-X object to the main node ("Settings") of the XML data file.
(setq oSettings (XML-Get-XMLObject "C:\\dwgset.xml"))

;;; Store an Active-X object to the "DrawingVars" node of the XML file.
(setq oDwgVars (XML-Get-Child oSettings nil "DrawingVars"))

;;; The following two lines return the exact same thing.
;;; An string object representing the value stored in the RegenAuto child node of the DrawingVars node.
(setq sRegenValue (XML-Get-Child-Value oDwgVars nil "RegenAuto"))
(setq sRegenValue (XML-Get-Child-Value oSettings "DrawingVars" "RegenAuto"))

;;; Return the value that is stored inside a "Color" attribute from the child node of Layers whose "Name" attribute = "Walls"
(setq color
  (read
    (XML-Get-Attribute
      (XML-Get-Child-ByAttribute oSettings "Layers" "Name" "Walls") "Color" "-1")
    )
  )
)

;;; if the value for 'color' that we got above is less than the zero (our supplied default argument was -1).
;;; then we'll set that attribute to 2.
(if (< color 0)
  (progn
    (XML-Put-Attribute (XML-Get-Child-ByAttribute oSettings "Layers" "Name" "Walls") "Color" 2)
    ;; After changing the Active-X object that is resident in memory,
    ;; we have to save all of our changes back to the XML file.
    (XML-Save oSettings)
  )
)

;;; release the objects
(vlax-release-object oSettings)
(vlax-release-object oDwgVars)

<?xml version="1.0"?>
<Settings>
  <Layers>
    <Layer Name="Walls" Color="2" LineType="continuous"/>
    <Layer Name="Furniture" Color="3" LineType="HIDDEN"/>
  </Layers>
  <DrawingVars>
    <RegenAuto>1</RegenAuto>
    <EdgeMode>0</EdgeMode>
    <Osmode>383</Osmode>
  </DrawingVars>
</Settings>
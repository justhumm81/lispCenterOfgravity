;;#################################################################################
;;#################################################################################
;;## CG3D (DEFINED AT THE BOTTOM OF THE file
;;## is the main function to determin the Center of Gravity of one or more
;;## AutoCAD solid elements.
;;##
;;## Thrown together haphazardly by AKH
;;## VERSION / DATE: 29 MAY 2025
;;#################################################################################
;;#################################################################################
;; This gives you access to symbolp, vl-princ-to-string, and other VL functions in any LISP environment.
;; it is loaded as soon as appload or startup script opens this LSP file
;; autocad still couldn't find the "vl-stringp" command for some reason
;;
(vl-load-com)
;;
;; ================================================================================
;; DEBUGGING CODE SYNTAX FOR LATER REFERENCE
;; ================================================================================
;; (prompt "\nC[DEBUG_POINT] A reached.") ;; DEBUGGING CHECKPOINT
;; (print hdrObjProp)  ;; DEBUGGING CHECKPOINT
;; (TypeSize hdrObjProp "hdrObjProp") ;; DEBUGGING
;; (prompt (strcat "\nVariable i = " (vl-prin1-to-string i))) ;; DEBUGGING
;; ================================================================================
;; Helper function definition to browse for folder with dialog box
;; ================================================================================
(defun BrowseFolder ( / objShell objFolder strPath)
  (vl-load-com)
  (setq objShell  (vlax-create-object "Shell.Application"))
  (setq objFolder (vlax-invoke-method objShell 'BrowseForFolder 0 "Select Folder" 0))
  (if objFolder
    (progn
      (setq strPath (vlax-get-property (vlax-get-property objFolder 'Self) 'Path))
      (vlax-release-object objShell)
      strPath
    )
    (progn
      (vlax-release-object objShell)
      nil
    )
  )
)
;; ================================================================================
;; ================================================================================

;; ================================================================================
;; ================================================================================
;; Save Array as text / csv file
;;
;; sample use:
;; (ArrayToTxt arrWeight)
;; ================================================================================
;; ================================================================================
(defun ArrayToTxt (arrData / strFilePath fileHandle row line)
	;; Show Save As dialog
	(setq strFilePath (getfiled "Save CSV As" "" "csv" 1))

	(if strFilePath
		(progn
			;; Open file for writing
			(setq fileHandle (open strFilePath "w"))

			;; Loop through each row in 2D array
			(foreach row arrData
				(setq line "")
				
				(foreach item row
				  
				  (setq line (strcat line (vl-prin1-to-string item) ","))
				  
				)
				
				;; Trim trailing comma and write line
				(write-line (vl-string-trim "," line) fileHandle)
				
			)

			;; Close file and return path
			(close fileHandle)
			strFilePath
			)
		;; ELSE statement. User canceled dialog
		nil
	)
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Debugging Helper Function that will get the size and type of an array 
;; and print it to the autocad console
;; sample use:
;; (TypeSize arrObjProp "arrObjProp") ;; [DEBUG TypeSize]
;; type out the string "arrObjProp" (or whatever) so it prints out while debugging
;; ================================================================================
;; ================================================================================
(defun TypeSize (thing name / typeName rowCount colCount)
  (cond
    ;; Not A List or Array
    ((not (listp thing))
     (prompt (strcat "\n[DEBUGTypeSize] " name " is not a list: " (vl-prin1-to-string thing)))
    )

    ;; Flat 1D List
    ((not (listp (car thing)))
     (setq typeName "list")
     (setq rowCount (length thing))
     (prompt (strcat "\n[DEBUGTypeSize] " name " is a " typeName " with length " (itoa rowCount)))
    )

    ;; 2D array 
    ((listp (car thing))
     (setq typeName "array")

     ;; Fallback in case arrOut is nil or malformed
     (if thing
       (setq rowCount (length thing))
       (setq rowCount 0)
     )

     ;; Fallback for malformed first row
     (if (and (car thing) (listp (car thing)))
       (setq colCount (length (car thing)))
       (setq colCount 0)
     )

     (prompt
       (strcat
         "\n[DEBUGTypeSize] "
         name " is an " typeName
         " with size "
         (itoa rowCount)
         " x "
         (itoa colCount)
       )
     )
    )
  )
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function to convert a 1D list/array to a 2D definition
;; that can be used as a wrapper
;; in case a calling function is expecting a 2D result
;; ================================================================================
;; ================================================================================
(defun To2D (arr1D)
  (mapcar '(lambda (x) (list x)) arr1D)
)


;; ================================================================================
;; ================================================================================
(defun To1D (arr2D / result)
  (cond
    ;; If it's a 1-row 2D list (like: (("A" "B" "C")))
    ((and (listp arr2D) (= (length arr2D) 1))
     (car arr2D)
    )

    ;; If it's a 1-column 2D list (like: (("A") ("B") ("C")))
    ((and (listp arr2D) (= (length (car arr2D)) 1))
     (mapcar 'car arr2D)
    )

    ;; If it's not 1-row or 1-column, return as-is or nil
    (T
     (prompt "\n[DEBUGARRAY] To1D: Input is not a 1-row or 1-column 2D array.")
     nil
    )
  )
)
;; ================================================================================
;; ================================================================================

;; ================================================================================
;; ================================================================================
;; Helper Function to search a list and return the index value of the matching element
;;
;; SAMPLE USE:
;; (setq hdrObjProp (list "ObjectID" "ObjectName" "Layer" "Density" "Volume" "Centroid X" "Centroid Y" "Centroid Z"))
;; (setq i (IndexOfMatch hdrObjProp "*DENS*")) ; case-insensitive search
;; ================================================================================
;; ================================================================================
(defun IndexOfMatch (arr pattern / i result)
	
	(setq i 0)
	(setq result nil)
  
	(cond
		;; it's not a list at all
		((not (listp arr))
			(setq typeName nil)
		)

		;; it's a 1D array or list
		;; this is okay, we don't need to do anything...continue...
		((not (listp (car arr)))
			 (setq typeName "list")
		)

		;; it's a 2D array
		;; need (at least temporarily) to be a 1D array or list
		((listp (car arr)) ; 2D array
			(setq typeName "array")
		)
	)
	
	(if (= "array" typeName)
		(setq arr (To1D arr))
	)
  
	(while (and arr (not result))
		(if (wcmatch (strcase (car arr)) (strcase pattern))
			(setq result i)
			(setq i (1+ i))
		)
		(setq arr (cdr arr))
	)
	result
)

;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; TRANSPOSE function to effectively conver a 2x7 array to a 7x2 array
;; should work for either 1D or 2D arrays
;; ================================================================================
;; ================================================================================
(defun Transpose (arr / rowCount colCount result)
  (cond
    ;; If 1D list, convert to vertical 2D
    ((not (listp (car arr)))
     (mapcar '(lambda (x) (list x)) arr)
    )

    ;; If 2D, transpose rows and columns
    ((listp (car arr))
     (setq colCount (length (car arr)))
     (setq result '())
     (repeat colCount
       (setq result
         (append result
           (list (mapcar '(lambda (row) (car row)) arr))
         )
       )
       ;; Strip first item off each row for next column
       (setq arr (mapcar 'cdr arr))
     )
     result
    )
  )
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function to Extract
;; from a 2D array, copy/extract a single column into a new array
;;
;; sample user
;; (setq arrNew (ExtractColumn arrData 1)) ; colIndex is 0-based
;; ================================================================================
;; ================================================================================
(defun ExtractColumn (arr2D colIndex / arrResult row)
  (setq arrResult '()) ; initialize result list
  (foreach row arr2D
    (setq arrResult (append arrResult (list (nth colIndex row))))
  )
  arrResult
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function to filter out non-unique values from a list / array
;; sample use:
;; (setq arrLayers (UniqueValues arrLayers))
;; ================================================================================
;; ================================================================================
(defun UniqueValues (arr / item arrUnique)
  (setq arrUnique '())
  (foreach item arr
    (if (not (member item arrUnique))
      (setq arrUnique (append arrUnique (list item)))
    )
  )
  arrUnique
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; Helper Function to filter out empty values from a list / array
;; intent is to use array as space holders
;;
;; sample use:
;; (setq arrBlank3x3 (BlankArray 3 3))
;; Result: (("" "" "") ("" "" "") ("" "" ""))
;;
;; ================================================================================
;; ================================================================================
(defun RemoveEmpty (arr / item arrNoEmpty)
  (setq arrNoEmpty '()) ; initialize as empty list
  (foreach item arr
    (if (/= item "") ; skip empty strings
      (setq arrNoEmpty (append arrNoEmpty (list item)))
    )
  )
  arrNoEmpty ; return cleaned array
)
;; ================================================================================
;; ================================================================================

;; ================================================================================
;; Helper Function that will create a blank array
;; Blank arrays can then be inserted into other arrays to change the size/dimensions.
;; sample use:
;; (setq arrClean (RemoveEmpty arrStuffed))
;; ================================================================================
;; ================================================================================
(defun BlankArray (r c / row arr i)
  ;; Create a single row with c blank entries
  (setq row (repeat c (setq arr (cons "" arr))) arr (reverse arr))
  
  ;; Create r copies of that row
  (setq arr '())
  (repeat r
    (setq arr (cons row arr))
  )
  (reverse arr)
)


;; ================================================================================
;; ================================================================================
;; Helper Function to Append one array (1D or 2D) onto the end of another array (1D or 2D)
;; sample use:
;; (setq arrNew (AppendColumn arrBig arrLittle))
;; arrBig → a 2D array (e.g., 6×4)
;; arrLittle → a 1D array (e.g., 6×1)
;; ================================================================================
;; ================================================================================
(defun AppendColumn (arr2D arr1D / arrNew rowA rowB isA2D isB2D)
  ;; Convert to 2D if either input is 1D
  (if (not (listp (car arr2D)))
    (setq arr2D (To2D arr2D))
  )
  (if (not (listp (car arr1D)))
    (setq arr1D (To2D arr1D))
  )

  ;; Dimension check
  (if (/= (length arr2D) (length arr1D))
    (progn
      (prompt "\nError: Appended Columns must have the same number of rows.")
      nil
    )
    (progn
      (setq arrNew '())
      (while (and arr2D arr1D)
        (setq rowA (car arr2D))
        (setq rowB (car arr1D))
        (setq arrNew (append arrNew (list (append rowA rowB))))
        (setq arr2D (cdr arr2D))
        (setq arr1D (cdr arr1D))
      )
      arrNew
    )
  )
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function to STACK / append a 2D array below another 2D array
;;
;; supposedly, LISP allows jagged arrays
;; ================================================================================
;; ================================================================================
(defun AppendRows (arrTop arrBottom / arrNew isTop2D isBot2D)
  ;; Ensure both inputs are 2D arrays
  (if (not (listp (car arrTop)))
    (setq arrTop (To2D arrTop))
  )
  (if (not (listp (car arrBottom)))
    (setq arrBottom (To2D arrBottom))
  )

  ;; Append arrBottom rows below arrTop
  (setq arrNew (append arrTop arrBottom))

  arrNew
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function definition to insert the columns of one Array
;; into / in between the columns of another array.
;; arrA is inserted into arrB
;; after columnIndex of arrB
;; sample use:
;; 			(setq arrObjProp (InsertColumns arrObjProp arrObjDens 2))
;; ================================================================================
;; ================================================================================
(defun nthcdr (n lst)
  (if (or (null lst) (<= n 0))
    lst
    (nthcdr (1- n) (cdr lst))
  )
)

;; Helper: range of integers from start to end inclusive
(defun range (start end / result)
  (if (> start end)
    nil
    (cons start (range (1+ start) end))
  )
)

(defun InsertColumns (arrA arrB indexB / rA cA rB cB arrNew rowA rowB left right rowNew i)

  ;; Coerce arrA if it's a scalar (atom) or a 1D list
  (cond
    ((atom arrA)                       ; atom → ((arrA))
     (setq arrA (list (list arrA))))
    
    ((and (listp arrA)
          (not (listp (car arrA))))    ; 1D list → ((val))
     (setq arrA (list arrA)))
  )

  ;; Get array dimensions
  (setq rA (length arrA))
  (setq rB (length arrB))

  ;; Assume rectangular data
  (setq cA (length (car arrA)))
  (setq cB (length (car arrB)))

  ;; Error if row counts don't match
  (if (/= rA rB)
    (progn
      (prompt "\nError: Inserted Columns must have the same number of rows.")
      nil
    )
    
    ;; Build new array
    (progn
      (setq arrNew '())
      (setq i 0)
      (repeat rB
        (setq rowA (nth i arrA))
        (setq rowB (nth i arrB))

        ;; Split rowB into left and right parts
        (setq left  (mapcar '(lambda (j) (nth j rowB)) (range 0 indexB)))
        (setq right (nthcdr (1+ indexB) rowB))

        ;; Build new row and append
        (setq rowNew (append left rowA right))
        (setq arrNew (append arrNew (list rowNew)))

        (setq i (1+ i))
      )
      arrNew
    )
  )
)

;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function definition to multiply two columns from arrays together
;; sample use:
;; (setq arrProduct (arrA 2 arrB 3))
;; ================================================================================
;; ================================================================================
(defun MultColumns (arrA colA arrB colB / arrProduct rowA rowB valA valB)
  ;; Coerce to 2D arrays
  (if (not (listp (car arrA))) (setq arrA (To2D arrA)))
  (if (not (listp (car arrB))) (setq arrB (To2D arrB)))

  ;; Dimension check
  (if (/= (length arrA) (length arrB))
    (progn
      (prompt "\nError: Multiplied Columns must have the same number of rows.")
      nil
    )
    (progn
      (setq arrProduct '())
      (while (and arrA arrB)
        (setq rowA (car arrA))
        (setq rowB (car arrB))

        (setq valA (nth colA rowA))
        (setq valB (nth colB rowB))

        ;; convert strings if necessary
        (setq valA (if (numberp valA) valA (read valA)))
        (setq valB (if (numberp valB) valB (read valB)))

        (setq arrProduct (append arrProduct (list (* valA valB))))

        (setq arrA (cdr arrA))
        (setq arrB (cdr arrB))
      )
      arrProduct
    )
  )
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function definition to sum columns of an array
;; sample use:
;; (setq totalMass (sumCol arrWeight 0))
;; ================================================================================
;; ================================================================================
(defun sumCol (arr colIndex / total row val)
  ;; If it's a 1D list, wrap it
  (if (not (listp (car arr)))
    (setq arr (To2D arr))
  )

  (setq total 0.0)
  (foreach row arr
    (setq val (nth colIndex row))
    (if (numberp val)
      (setq total (+ total val))
      (setq total (+ total (read val))) ; just in case
    )
  )
  total
)

;; ================================================================================
;; ================================================================================

;; ================================================================================
;; ================================================================================
;; Helper Function to Make sure layer exists
;; sample use:
;; (LayerExists "CGsphere")
;; ================================================================================
;; ================================================================================
(defun LayerExists (layerName / layObj)
  (if (not (tblsearch "LAYER" layerName))
    (progn
      (setq layObj
        (vla-add
          (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object)))
          layerName
        )
      )
      (vla-put-Color layObj 1) ; 1 = red
    )
  )
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function to draw a sphere
;; sample use:
;; (DrawSphere centX centY centZ 12 "CGsphere")
;; ================================================================================
;; ================================================================================
(defun DrawSphere (cenX cenY cenZ radius layerName / acadDoc modelSpace sphere ptCenter)

	(setq acadDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
	(setq modelSpace (vla-get-ModelSpace acadDoc))

	;; Create center point
	(setq ptCenter (vlax-3d-point (list cenX cenY cenZ)))

	;; Create the sphere
	(setq sphere (vla-AddSphere modelSpace ptCenter radius))

	;; Set layer
	(vla-put-Layer sphere layerName)
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function definition to check the format of user input
;; ================================================================================
;; ================================================================================
(defun FormatNumbers (arr / result)
	(setq result '())

	(foreach item arr
		(cond
			;; If it's already a number (int or real), keep it
			((or (= (type item) 'INT) (= (type item) 'REAL))
				(setq result (append result (list item)))
			)

			;; IF ITEM IS A STRING
			((= (type item) 'STR)
			 (cond
			   ;; see if it can be read ast a number
			   ((numberp (read item))
				(setq result (append result (list (read item))))
			   )
			   ;; if can't read string, see if it's a fraction
			   ((ParseFracStr item)
				(setq result (append result (list (ParseFracStr item))))
			   )
			   (T
				(prompt (strcat "\n[DEBUGFORMAT] Could not convert string: " item))
				(setq result (append result (list "STR")))
			   )
			 )
			)

      ;; If it's a symbol, try to convert from its name
      ((= (type item) 'SYM)
       (if (numberp (read (symbol-name item)))
         (setq result (append result (list (read (symbol-name item)))))
         (progn
           ;; conversion failed
           (prompt (strcat "\n[DEBUG] Could not convert symbol: " (symbol-name item)))
           (setq result (append result (list "SYM")))
         )
       )
      )

      ;; Fallback for unknown types
      (T
       (prompt "\n[DEBUGFORMAT] Unknown type encountered.")
       (setq result (append result (list 0)))
      )
    )
  )

  result
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function to parse a fraction stored in a string
;; into a REAL number (..."3/4" --> 0.750)
;; ================================================================================
;; ================================================================================
(defun ParseFracStr (s / idx num den)
	;; Make sure s is a string before continuing
	(if (/= (type s) 'STR)
		nil ; exit early — not a string
	(progn
		;; Now we know s is a string, check if it's a valid fraction
		(if (and (> (strlen s) 2) (/= (vl-string-search "/" s) nil))
			(progn
				(setq idx (vl-string-search "/" s))
				(setq num (substr s 1 idx))
				(setq den (substr s (+ idx 2)))

			(if (and (numberp (read num)) (numberp (read den)))
				;; LISP does "integer devision", 3/4 = 1
				;; convert integers to floating point numbers
				(/ (float (read num)) (read den))
				nil ; failed to convert numerator or denominator
			)
        )
        nil ; no "/" found or string too short
      )
    )
  )
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Popup  -  Lee Mac
;; A wrapper for the WSH popup method to display a message box prompting the user.
;; ttl - [str] Text to be displayed in the pop-up title bar
;; msg - [str] Text content of the message box
;; bit - [int] Bit-coded integer indicating icon & button appearance
;; Returns: [int] Integer indicating the button pressed to exit
;;
;; BUTTONS
;; 0	Display OK button
;; 1	Display OK and Cancel buttons
;; 2	Display Abort, Retry, and Ignore buttons.
;; 3	Display Yes, No, and Cancel buttons.
;; 4	Display Yes and No buttons.
;; 5	Display Retry and Cancel buttons.
;; 6	Display Cancel, Try Again, and Continue buttons.
;; 
;; RETURN VALUES
;; 1	OK button
;; 2	Cancel button
;; 3	Abort button
;; 4	Retry button
;; 5	Ignore button
;; 6	Yes button
;; 7	No button
;; 10	Try Again button
;; 11	Continue button
;;
;; http://lee-mac.com/popup.html
;; see webpage for bit-codes and return definitions
;;
;; Example Function Call
;; (LM:popup "Title Text" "This is a test message." (+ 2 48 4096))
;; ================================================================================
;; ================================================================================
(defun LM:popup ( ttl msg bit / wsh rtn )
    (if (setq wsh (vlax-create-object "wscript.shell"))
        (progn
            (setq rtn (vl-catch-all-apply 'vlax-invoke-method (list wsh 'popup msg 0 ttl bit)))
            (vlax-release-object wsh)
            (if (not (vl-catch-all-error-p rtn)) rtn)
        )
    )
)
;; ================================================================================
;; ================================================================================

;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;;
;; subroutine to build array of selected element properties
;;
;; Loop through selected 3D solids, extract properties, and save them to an array
;; Each element in the selection set becomes its own row,
;; and each row contains values for 7 columns as noted
;; ================================================================================
;; ================================================================================
(defun subGetProps (ss / ent solAcad arr)

	;; create empty array to be filled in
	(setq arr '())
		
	(while (> (sslength ss) 0)
		(setq ent (ssname ss 0))
		(setq solAcad (vlax-ename->vla-object ent))

		;; Add this object's properties to the array
		(setq arr
			(append arr
				(list (list
					(vlax-get solAcad 'ObjectID)
					(vlax-get solAcad 'ObjectName)
					(vlax-get solAcad 'Layer)
					(vlax-get solAcad 'Volume)
					(car   (vlax-get solAcad 'Centroid))
					(cadr  (vlax-get solAcad 'Centroid))
					(caddr (vlax-get solAcad 'Centroid))
					)
				)
			)
		)

		;; Remove that entity from the selection set
		(setq ss (ssdel ent setSolids))
	)
	
	arr ;; return the 2D array of properties to the main routine

)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;;
;; Function to call up a dialog box definition.
;; calls a seperate dialog box file (*.DCL)
;;
;; This defun declaration defines local variables that won't go outside of this function...NO GOOD
;; (defun inputMaterial ( / dcl_id mat1 mat2 mat3 mat4 mat5 mat6 result)
;; Remove those declarations to allow global variable definitions elsewhere...
;; ================================================================================
;; ================================================================================
(defun subDialog ( / dcl_id result)

	;; Load DCL
	;; DCL should be saved in the same folder as this LISP
	;; Hardcode the folder path when the DCL is not in an autocad support folder
	;; double-backslashes in filepath strings
	;; (setq dcl_id (load_dialog "GetCGmaterials.dcl"))
	(setq dcl_id (load_dialog "C:\\Users\\ahummers\\OneDrive\\AKH_CADD\\_LISP\\CG working\\CG3Dmaterials.dcl"))
	(if (not (new_dialog "UserInputBoxes" dcl_id))
		(progn
			(alert "DCL failed to load.")
			(exit)
		)
	)
	
	(if arrLayers
		(progn
			;; disable (1) input all boxes 
			(mode_tile "mat1" 1)
			(mode_tile "mat2" 1)
			(mode_tile "mat3" 1)
			(mode_tile "mat4" 1)
			(mode_tile "mat5" 1)
			(mode_tile "mat6" 1)
			
			;; enable (change mode_tile to 0) input boxes IF material / layer exists
			(if (nth 0 arrLayers) (set_tile "label1" (nth 0 arrLayers)) )
			(if (nth 0 arrLayers) (mode_tile "mat1" 0) )
			(if (nth 1 arrLayers) (set_tile "label2" (nth 1 arrLayers)) )
			(if (nth 1 arrLayers) (mode_tile "mat2" 0) )
			(if (nth 2 arrLayers) (set_tile "label3" (nth 2 arrLayers)) )
			(if (nth 2 arrLayers) (mode_tile "mat3" 0) )
			(if (nth 3 arrLayers) (set_tile "label4" (nth 3 arrLayers)) )
			(if (nth 3 arrLayers) (mode_tile "mat4" 0) )
			(if (nth 4 arrLayers) (set_tile "label5" (nth 4 arrLayers)) )
			(if (nth 4 arrLayers) (mode_tile "mat5" 0) )
			(if (nth 5 arrLayers) (set_tile "label6" (nth 5 arrLayers)) )
			(if (nth 5 arrLayers) (mode_tile "mat6" 0) )
		)
	)

	;; Accept button
	;; set global variables, so they're sent to the calling method/routine
	;; UPDATE this list if the dialog ever changes
	(setq arrMatKeys '("mat1" "mat2" "mat3" "mat4" "mat5" "mat6" "SphLay" "SphDiam"))
	
	;; this is generating a string to send to the DCL engine, which will be turned INTO
	;; an action when the accept button is clicked. And the arrOut array is created.
	(action_tile "accept"
		(strcat
			"(progn "
				"(setq arrOut (mapcar 'get_tile arrMatKeys)) "
				"(done_dialog 1))"
		)
	)

	;; Cancel button
	(action_tile "cancel" "(done_dialog 0)")

	;; Start the dialog
	(setq result (start_dialog))
	(unload_dialog dcl_id)

	;; remove empty values from the array
	(setq arrOut (RemoveEmpty arrOut))

	;; assign last two elements of arrOut to sphere variables.
	;; and delete the last two rows/elements from the array
	;; (reverse arrOut) → reverses the list so the last two elements are now at the front.
	;; (cdr (cdr ...)) → removes the first two elements of the reversed list (i.e., the original last two).
	;; reverse again to restore original order minus the last two items.	
	(setq i (length arrOut)); i = 4
	(setq strCGLayer (nth (- i 2) arrOut))    ; (nth 2 arrOut) = "CGsphere" (a layer name)
	(setq diaSphere  (nth (- i 1) arrOut))    ; (nth 3 arrOut) = "4" (a number)
	(setq arrOut (reverse (cdr (cdr (reverse arrOut)))))
	
	;; make sure all of the input values are valid
	;; this is sent back to the calling routine
	(setq arrOut (FormatNumbers arrOut))
	
	;; Bundle multiple values to be returned to calling routine
	(list arrOut strCGLayer diaSphere)

)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;; this creates a new 1D list/array of material desity
;; that matches up to the selected objects (array) 
;; ================================================================================
;; ================================================================================
(defun subObjDensity (arrP hdr arrD / arrOut rowObj i objLayer objDens arrScan)

	;; create empty array...will be 1D array of density values
	(setq arrOut '())
	
	;; check which column "Layer" is in
	(setq i (IndexOfMatch hdr "*Layer*"))
	
	;; Loop Through each object row
	(foreach rowObj arrP
		
		(setq objLayer (nth i rowObj))
		(setq objDens "") ;; default if not found
		
		;; nested loop through arrD to find match
		;; save to local arrScan array for scratch work
		(setq arrScan arrD)
		
		(while arrScan
			;; caar operator shouild get the first element of a list (array?)
			(if (= objLayer (caar arrScan))  ;; match layer name
				(progn
					(setq objDens (cadar arrScan)) ;; get matching density
					(setq arrScan nil) ; this breaks the loop
				)
				(setq arrScan (cdr arrScan))
			)
		)
		;; append the new row with density
		(setq arrOut (append arrOut (list (list objDens))))
	)
	
	;; send object dnsity array back to main routine
	arrOut

)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; MATH SUBROUTINE
;; called by the main function / routine (below)
;;
;; (setq arrCG (subMath hdrObjProp hdrPropAdd arrObjProp))
;; 
;; ================================================================================
;; ================================================================================
(defun subMath (arrDwg hdrP hdrPA arr / i j k arrWeight
							arrWX arrWY arrWZ totWt
							totWX totWY totWZ
                            hdrTotals arrTot
                            centX centY centZ
                            hdrCent arrCent arrOut)
  
	(setq i (IndexOfMatch hdrP "*DENS*"))
	(setq j (IndexOfMatch hdrP "*VOL*"))
  
	(setq arrV (ExtractColumn arr j))
	(setq arrWeight (MultColumns arr i arr j))
  
	(setq k (IndexOfMatch hdrP "*Cent*X*"))
	(setq arrWX (MultColumns arrWeight 0 arr k))
  
  	(setq k (IndexOfMatch hdrP "*Cent*Y*"))
	(setq arrWY (MultColumns arrWeight 0 arr k))
  
	(setq k (IndexOfMatch hdrP "*Cent*Z*"))
	(setq arrWZ (MultColumns arrWeight 0 arr k))
  
	(setq arr (AppendColumn arr arrWeight))
	(setq arr (AppendColumn arr arrWX))
	(setq arr (AppendColumn arr arrWY))
	(setq arr (AppendColumn arr arrWZ))
  
	(setq totV (sumCol arrV 0)) ;; NEED TO INSERT 3 BLANK COLUMNS AFTER VOLUME
	(setq totWt (sumCol arrWeight 0))
	(setq totWX (sumCol arrWX 0))
	(setq totWY (sumCol arrWY 0))
	(setq totWZ (sumCol arrWZ 0))
  
	;; column headers for the totals
	(setq titlTot "SUM TOTALS")
	(setq hdrTotals (Transpose(To2D (list "Tot. Vol." "Tot. Weight" "Tot. WX" "Tot. WY" "Tot. WZ"))))
	(setq arrTot (Transpose(To2D (list totV totWt totWX totWY totWZ))))
	
	(setq centX (/ totWX totWt))
	(setq centY (/ totWY totWt))
	(setq centZ (/ totWZ totWt))
  
	;; column headers for the centroid coordinates
	(setq titlCent "CG COORDINATES")
	(setq hdrCent (To2D (list "Centroid X" "Centroid Y" "Centroid Z")))
	(setq hdrCent (Transpose hdrCent))
	(setq arrCent (To2D (list centX centY centZ)))
	(setq arrCent (Transpose arrCent))
  
	;; titlTot is just a STRING
	;; wrap it in a list to turn it into a 2D array that can be appended to other arrays
	(setq TitlTot (To2D (list titlTot)))
	(setq titlCent (To2D (list titlCent)))
  
	;; NEED TO APPEND/INSERT hdrPA after hdrA.
	(setq hdrPA (Transpose (To2D hdrPA))) ;; make the additional header list into an array and transpose it
	(setq hdrP (AppendColumn hdrP hdrPA))  ; insert hdrPA at the end
  
	;; STACK / append Total and Center rows for later insertion into arrOut
	(setq arrTot (AppendRows hdrTotals arrTot))
	(setq arrCent (AppendRows hdrCent arrCent))
	
	;; Define blank arrays to cushion Totals and Centers
	(setq Blank2x4 (BlankArray 2 4))
	(setq Spacer2x3 (BlankArray 2 3))
	(setq Blank2x9 (BlankArray 2 9))
  
	(setq arrTot (InsertColumns Spacer2x3 arrTot  0)) ;; after colIndex 0
	(setq arrTot (AppendColumn Blank2x4 arrTot))
	(setq arrCent (AppendColumn Blank2x9 arrCent))
	
	(setq arrOut arrDwg) ;; insert drawing info into arrOut
  
	;; STACK / append arrays to be output to CSV
	(setq arrOut (AppendRows arrOut hdrP))
	(setq arrOut (AppendRows arrOut arr))
	(setq arrOut (AppendRows arrOut titlTot))	
	(setq arrOut (AppendRows arrOut arrTot))
	(setq arrOut (AppendRows arrOut titlCent))
	(setq arrOut (AppendRows arrOut arrCent))
  
	;; send output array back to main ROUTINE
	arrOut
  
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;; This gets a list of the unique layers in the selection set
;; Unique layers names are then sent to the dialog box as labels
;; 
;; ================================================================================
;; ================================================================================
(defun subGetLay (num arr hdr)
	;; Count number of 3D solids in selection
	;; and display a popup message with the 
	;; total number of objects selected and the number of solid objects selected
	(setq j 0 i 0)
	(while (< i num)
		(if (equal (cadr (nth i arr)) "AcDb3dSolid")
		(setq j (1+ j)))
		(setq i (1+ i))
	)

	(setq strMsg (strcat "Total Objects Selected: " (itoa num)
					"\n\nNumber of 3D Solid Objects: " (itoa j)
					"\n\nDo you wish to continue?"
				)
	)	

	(setq intResult (LM:popup "Confirm Selection" strMsg (+ 1 32 4096))) ; OK-Cancel, Question Mark
	(if (= intResult 1)
		(progn
			;; OK was pressed - continue with rest of routine
			(princ "\nUser pressed OK. Continuing...")
			;; your code here / continue with program
		)
		;;ELSE
		(progn
			;; Cancel was pressed - exit the function
			(princ "\nUser pressed Cancel. Aborting.")
			(exit) ; or (quit) if you want to fully bail out
		)
	)
		
	;; Extract (unique) layers from arrObjProp and eliminate non-unique layer names
	(setq i (IndexOfMatch hdr "*Layer*"))
	
	(setq arrOut (ExtractColumn arr i)) ; colIndex is 0-based
	(setq arrOut (UniqueValues arrOut))			
	
	;; send arrOut back to calling routine
	arrOut
)
;; ================================================================================
;; ================================================================================

;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;; This gets the general material density from user input
;; 
;; ================================================================================
;; ================================================================================
(defun subGetDens (arrL arrInput)

	;; Check if user clicked Cancel (returns nil) on the dialog box
	(if arrInput
		(progn					
			;; Use arrInput values here
			;; (e.g., (nth 0 arrInput) = first input box value)
			(princ "\nUser dialog box input collected successfully.")
		)
		;;ELSE
		(progn
				(princ "\nUser canceled input dialog box. Aborting.")
				(exit) ; or (quit) or just skip the rest of the function
		)
	)
	
	;; get the dialog box values for the material densities
	;; append the layer name and the layer density into a single 2D array
	;; arrD is created via the dialog box and subroutine
	(setq arrOut (AppendColumn arrL arrInput))

	;;send data to a text CSV file
	;;(setq strFile (ArrayToTxt arrOut)) ; [DEBUG Write Array]

	;; return density array to main routine
	arrOut	
)
;; ================================================================================
;; ================================================================================


;;#################################################################################
;;#################################################################################
;;
;; THIS IS THE MAIN ROUTINE THAT CALLS THE ABOVE HELPER FUNCTIONS
;;
;;#################################################################################
;;#################################################################################
(defun c:CG3D ()
  
	;; Clear any existing selection set
  	(if (ssget "_P") (setq setSolids nil))
	
	(prompt "\n[DEBUG_POINT] 1 reached")
  
	;; Capture current date and time
  	(setq dateNow (rtos (getvar "CDATE") 2 6))
  
	;; Get current drawing name
	(setq strDwgName (getvar "DWGNAME"))

	;; --------------------------------------------------------------------------------
	;; Get unit system
	;; --------------------------------------------------------------------------------
	(setq intUnitSys (getvar "INSUNITS"))
	(setq arrUnits '("Unspecified" "Inches" "Feet" "Miles" "Millimeters" "Centimeters" "Meters" "Kilometers"
                   "Microinches" "Mils" "Yards" "Angstroms" "Nanometers" "Microns" "Decimeters"
                   "Dekameters" "Hectometers" "Gigameters" "Astronomical Units" "Light Years" "Parsecs"
                   "US Survey Feet" "US Survey Inch" "US Survey Yard" "US Survey Mile")
	)
  
	;; Convert the unit system number to a string
	(setq strUnitSys (if (and (>= intUnitSys 0) (<= intUnitSys 25))
                        (nth intUnitSys arrUnits)
                        "Unknown")
	)
  
	;; --------------------------------------------------------------------------------
	;; can consolidate these notices with LeeMac messages or something
	;; uncomment when all other debugging is completed
	;; --------------------------------------------------------------------------------
	;; Display drawing info in pop up message box
	;;(alert (strcat "Date: " dateNow "\nDwg. Name: " strDwgName "\nUnits: " strUnitSys))
	;; Prompt user to select objects in pop up message box
	;;(alert "Select Objects! Hit Enter to Finish!")
	(setq strMsg (strcat "Dwg. Name: " strDwgName
					"\n\nDate: " dateNow
					"\n\nDwg. Units: " strUnitSys
					"\n\nSelect Solid Objects."
					"\nPress ENTER when selection complete.")
	)
	
	(setq arrDwg (list
               (list "Dwg. Name:" strDwgName)
               (list "Date:"      dateNow)
               (list "Units:"     strUnitSys)
             ))

	
	;; general notification message to let user know calculator is starting
	;; allow to OK or CANCEL
	(setq intResult (LM:popup "Center of Gravity Calculator" strMsg (+ 1 32 4096))) ; OK-Cancel, Question Mark
	(if (= intResult 1)
		(progn
			;; OK was pressed - continue with rest of routine
		)
		;;ELSE
		(progn
			;; Cancel was pressed - exit the function
			(princ "\nUser pressed Cancel. Aborting.")
			(exit) ; or (quit) if you want to fully bail out
		)
	)
	
	(prompt "\n[DEBUG_POINT] 2 reached")
	
	;; request the user to make a selection
	;; (setq setSolids (ssget))
	(setq setSolids (ssget '((0 . "3DSOLID"))))

  
	;; --------------------------------------------------------------------------------
	;; Main "IF" block that will call and process subroutines
	;; --------------------------------------------------------------------------------
	(if setSolids
		
   		;; --------------------------------------------------------------------------------
		;; TRUE condition fo IF statement
		;; --------------------------------------------------------------------------------
   		(progn

			(prompt "\n[DEBUG_POINT] 3 reached")

			(setq numObj (sslength setSolids))
			
			;;(princ (strcat "\nNumber of 3D Solids selected: " (itoa numObj)))

			;; Define a header row for later of object properties for later use
			;; these should match the append of arrObjProp, below.
			(setq hdrObjProp (list "ObjectID" "ObjectName" "Layer" "Volume" "Centroid X" "Centroid Y" "Centroid Z"))
			
			;; additional column headers to append to the existing properties array(s)
			(setq hdrPropAdd (list "Weight" "W * X" "W * Y" "W * Z"))
	
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to create array of object properties ;;
			;; create arrObjProp                                    ;;
			;; (setq variable (subroutine argument1 argument2))
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(setq arrObjProp (subGetProps setSolids))

			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to create an array of Layers        ;;
			;; of selected solids                                  ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(setq arrLayers (subGetLay numObj arrObjProp hdrObjProp))
			
			(prompt "\n[DEBUG_POINT] 4 reached")
			
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call the input dialog box via a subroutine   ;;
			;; and then unpack the multiple returned values ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;(setq arrUserInput (subDialog))
			(progn
				(setq tmpReturn (subDialog))
				(setq arrUserInput (nth 0 tmpReturn)
						strCGLayer (nth 1 tmpReturn)
						diaSphere  (nth 2 tmpReturn))
			)
			
			;;(prompt (strcat "\n[DEBUG] strCGLayer = " (vl-prin1-to-string strCGLayer)))
			;;(prompt (strcat "\n[DEBUG] diaSphere = " (vl-prin1-to-string diaSphere)))
			
			(prompt "\n[DEBUG_POINT] 5 reached")
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to create an array of user Input    ;;
			;; material densities                                  ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(setq arrDensity (subGetDens arrLayers arrUserInput))
			
			(prompt "\n[DEBUG_POINT] 6 reached")
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to match element layers & densities ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;			
			(setq arrObjDens (subObjDensity arrObjProp hdrObjProp arrDensity))
			
			;; Header of Object Properties is still a 1D list at this point, convert to array
			(setq hdrObjProp (To2D hdrObjProp))
	
			;; Header of Objects is not a 2D array with (multiple) rows x 1 column
			;; need to transpose those into 1 row x (multiple) column
			(setq hdrObjProp (Transpose hdrObjProp))
	
			;; insert object densities array (column) after the "Layer" column
			(setq i (IndexOfMatch hdrObjProp "*Layer*"))
			(setq j (To2D (list "Density")))

			(setq hdrObjProp (InsertColumns j hdrObjProp i)) ;; insert j after hdr
			(setq arrObjProp (InsertColumns arrObjDens arrObjProp  2)) ;; insert arrObDens after the 2nd col index, 3rd column
	
			(prompt "\n[DEBUG_POINT] 7 reached")
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to do math stuff ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(setq arrCG (subMath arrDwg hdrObjProp hdrPropAdd arrObjProp))

			(LM:Popup "Object Property Array Contents" (vl-prin1-to-string arrCG) (+ 1 32 4096)) ; OK-Cancel, Question Mark
			
			;; Get last row of arrCG
			;; Extract X, Y, Z from that row
			(setq arrCent (last arrCG))
			(setq centX (nth 9 arrCent))  ; column 10
			(setq centY (nth 10 arrCent)) ; column 11
			(setq centZ (nth 11 arrCent)) ; column 12
			
			(prompt "\n[DEBUG_POINT] 8 reached")
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to drawsphere    ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(LayerExists strCGLayer)
			
			(DrawSphere centX centY centZ diaSphere strCGLayer)
	
			;; LEAVE THIS HERE...this is the final "production" output
			;;send data to a text CSV file
			(setq strFile (ArrayToTxt arrCG))
			
			(prompt "\n[DEBUG_POINT] 9 reached")
			
		)
      	;; ----- END OF PROGRN -------------------------------------------------------------
   
   		;; --------------------------------------------------------------------------------
		;; FALSE/ELSE condition fo IF statement
		;; --------------------------------------------------------------------------------
		(progn
		
			(alert "No 3D Solids selected.
				\nRoute required as least one solid to be selected.
				\nRoutine will now be aborted."
			)
	   
			(prompt "\n[DEBUG_POINT] CGz reached")
		
		)
	
	)
	;; --------------------------------------------------------------------------------
	;; END OF Main "IF" block started above
	;; --------------------------------------------------------------------------------
	
  (princ)
)
;; ================================================================================
;; ================================================================================


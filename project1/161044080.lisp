(defvar *lexeme*)
(defvar *output*)
(defvar *nextChar*)
(defvar *charClass*)
(defconstant LETTER 0)
(defconstant DIGIT 1)
(defconstant UNKOWN 2)
(defconstant KEY_AND "KEY_AND")
(defconstant KEY_OR "KEY_OR")
(defconstant KEY_NOT "KEY_NOT")
(defconstant KEY_EQU "KEY_EQUAL")
(defconstant KEY_APP "KEY_APPEND")
(defconstant KEY_CON "KEY_CONCAT")
(defconstant KEY_SET "KEY_SET")
(defconstant KEY_DEF "KEY_DEFFUN")
(defconstant KEY_FOR "KEY_FOR")
(defconstant KEY_WHILE "KEY_WHILE")
(defconstant KEY_IF "KEY_IF")
(defconstant KEY_EXIT "KEY_EXIT")
(defconstant IDENT "IDENT")
(defconstant INT_LIT "INT_LITERAL")
(defconstant OP_+ "OP_+")
(defconstant OP_- "OP_-")
(defconstant OP_/ "OP_/")
(defconstant OP_* "OP_*")
(defconstant OP_LPA "OP_(")
(defconstant OP_RPA "OP_)")
(defconstant OP_** "OP_**")
(defconstant OP_´ "OP_'")
(defconstant KEY_T "KEY_TRUE")
(defconstant KEY_F "KEY_FALSE")
(defconstant KEY_NULL "KEY_NULL")


(defun lexer (fileName)
  (setf *output* nil)
  (with-open-file (stream fileName :if-does-not-exist nil)
    (if stream
        (progn
          (parser stream)
          (format t "(~{~{(~a ~s)~}~^, ~})" *output*))
        (format t "File \"~a\" does not exist!" fileName))))

(defun parser (stream)
  (getChar stream)
  (if *nextChar*
      (progn 
        (let ((token))
          (setf token (lex stream))
          (when token
            (nreverse *output*)
            (push (list token *lexeme*) *output*)
            (nreverse *output*)))
        (parser stream))))

(defun getChar (stream)
  (setf *nextChar* (read-char stream nil))
  (if *nextChar*
      (progn
        (cond
          ((alpha-char-p *nextChar*)
           (setf *charClass* LETTER))
          ((digit-char-p *nextChar*)
           (setf *charClass* DIGIT))
          (t
           (setf *charClass* UNKOWN))))))

(defun getNonBlank(stream)
  (when *nextChar*
    (if (or (char-equal *nextChar* #\space) (char-equal *nextChar* #\newline)
            (char-equal *nextchar* #\tab))
        (progn
          (getChar stream)
          (getNonBlank stream)))))
      
  
(defun takeLetter (stream)
  (setf *lexeme* (concatenate 'string *lexeme* (string *nextChar*)))
  (getChar stream)
  (when *nextChar*
    (cond
      ((equal *charClass* LETTER)
       (takeLetter stream))
      ((equal *charClass* DIGIT)
       (setf *lexeme* (concatenate 'string *lexeme* (string *nextChar*)))
       (error "Identifier ~s contains digit" *lexeme*))
      (t (file-position stream (1- (file-position stream)))))))

(defun takeDigit (stream)
  (setf *lexeme* (concatenate 'string *lexeme* (string *nextChar*)))
  (getChar stream)
  (when *nextChar*
    (cond
      ( (equal *charClass* DIGIT)
       (takeDigit stream))
      ((equal *charClass* LETTER)
       (setf *lexeme* (concatenate 'string *lexeme* (string *nextChar*)))
       (error "Number ~s contains letter" *lexeme*))
      (t (file-position stream (1- (file-position stream)))))))
  
(defun lookUp (stream)
  (setf *lexeme* (string *nextChar*))
  (cond
    ((char-equal *nextChar* #\+)
     OP_+)
    ((char-equal *nextChar* #\-)
     OP_-)
    ((char-equal *nextChar* #\/)
     OP_/)
    ((char-equal *nextChar* #\()
     OP_LPA)
    ((char-equal *nextChar* #\))
     OP_RPA)
    ((char-equal *nextChar* #\')
     OP_´)
    ((char-equal *nextChar* #\*)
     (getChar stream)
     (if (char-equal *nextChar* #\*)
         (progn
           (setf *lexeme* (concatenate 'string *lexeme* (string *nextChar*)))
           OP_**)
         (progn
           (file-position stream (1- (file-position stream)))
           OP_*)))
    (t (error "Character ~s is not defined in g++" *lexeme*))))

(defun lex (stream)
  (getNonBlank stream)
  (setf *lexeme* "")
  (when *nextChar*
    (cond
      ((equal *charClass* LETTER)
       (takeLetter stream)
       (cond
         ((string-equal *lexeme* "and") KEY_AND)
         ((string-equal *lexeme* "or") KEY_OR)
         ((string-equal *lexeme* "not") KEY_NOT)
         ((string-equal *lexeme* "equal") KEY_EQU)
         ((string-equal *lexeme* "append") KEY_APP)
         ((string-equal *lexeme* "concat") KEY_CON)
         ((string-equal *lexeme* "set") KEY_SET)
         ((string-equal *lexeme* "deffun") KEY_DEF)
         ((string-equal *lexeme* "for") KEY_FOR)
         ((string-equal *lexeme* "while") KEY_WHILE)
         ((string-equal *lexeme* "if") KEY_IF)
         ((string-equal *lexeme* "exit") KEY_EXIT)
         ((string-equal *lexeme* "true") KEY_T)
         ((string-equal *lexeme* "false") KEY_F)
         ((string-equal *lexeme* "null") KEY_NULL)
         (t IDENT)))

      ((equal *charClass* DIGIT)
       (takeDigit stream)
       INT_LIT)
      ((equal *charClass* UNKOWN)
       (lookUp stream)))))



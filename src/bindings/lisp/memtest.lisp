#|
\file    memtest.lisp
\brief   Memory test functions to check free-operations in the FFI
\author  Martin Ginkel <mginkel@mpi-mageburg.mpg.de>

$Id$
$Source$

Copyright 2004 Max-Planck-Institute Magdeburg

This is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published
by the Free Software Foundation; either version 2.1 of the License, or
any later version.

You should have received a copy of the GNU Lesser General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.

THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
|#


(in-package :libsbml)

(defun put-1-string (doc)
  (let ((s (make-string (round (/ array-dimension-limit 1000)) :initial-element #\+))
	(m (sbmldocument-get-model doc)))
    (species-set-name (model-create-species m) s)
    )
  )
(defun make-ast (count)
  (let ((ast1 (astnode-create-with-type +ast-real+))
	(ast))
    (astnode-set-real ast1 1.0d0)
    (cond ((= 0 count) ast1)
	  (t (setf ast (astnode-create-with-type +ast-plus+))
	     (astnode-add-child ast ast1)
	     (astnode-add-child ast (make-ast (1- count)))
	     ast)
	  )
    )
  )

(defun convert-1-formula ()
  "test: create  formula convert and free both"
  (let* ((ast (make-ast 100))
	 (string (sbml-formula-to-string ast)))
    (astnode-free ast)
    string)
  )
(defun convert-formula-2000 ()
  "test: create 1 formula, convert it 100 times"
  (let* ((ast (make-ast 10000))
	 (string))
    (dotimes (i 20000)
      (setf string (sbml-formula-to-string ast)))
    (astnode-free ast)
    )
  )

(defun get-1-string (doc)
  (let (
	(m (sbmldocument-get-model doc)))
    (species-get-name (model-get-species m 0))
    )
  )

(defun make-doc ()
  (let ((d (sbmldocument-create)))
    (sbmldocument-set-model d (model-create))
    d)
  )

(defun free-doc (doc)
  (sbmldocument-free doc)
  )

(defun write-read ()
  (let ((d (make-doc))
	  s)
    (unwind-protect
	(progn 
	  (format nil ".")
	  (put-1-string d)
	  (dotimes (i 10)
	    (setf s (get-1-string d))
	    ))
      (free-doc d)
    ))
  )
(defun test-write-read (&optional (n 10000))
  (dotimes (i n)
    (write-read))
  )
(defun test-formulas (&optional (n 50000))
  (dotimes (i n)
    (convert-1-formula)
    )
  )

(defun test-formula-2000 ()
  "test: create 1 formula, convert it 100 times"
  (let* ((ast (make-ast 5000))
	 (string))
    (dotimes (i 2000)
      (setf string (sbml-formula-to-string ast)))
    (astnode-free ast)
    )
  )

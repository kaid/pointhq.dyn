(ql:quickload 'drakma)
(ql:quickload 'cl-interpol)
(ql:quickload 'ip-interfaces)
(ql:quickload 'optima)
(ql:quickload 'alexandria)

(import 'drakma:http-request)
(import 'ip-interfaces:get-ip-interfaces)
(import 'ip-interfaces::ip-interface-name)
(import 'ip-interfaces::ip-interface-address)
(import 'alexandria:flatten)
(import 'optima:match)
(cl-interpol:enable-interpol-syntax)

(defparameter *info-file* "~/.pointhq.dyn")

(defun vector-to-ip (vector)
	(format nil "~{~d~^.~}" (coerce vector 'list)))

(defun ethernet-interfacep (interface)
	(match (ip-interface-name interface)
		((or "eth0" "en0") interface)))

(defun current-ip ()
	(let* ((interface
					 (find-if
						#'ethernet-interfacep
						(get-ip-interfaces)))
				 (ip-vector (and interface (ip-interface-address interface))))
		(vector-to-ip ip-vector)))

(defun prompt-and-pair (string)
	(format *standard-output* "~(~a~): " string)
	(list string (read-line)))

(defun sync-record (&key record-id username api-token zone-id)
	(http-request #?"https://pointhq.com/zones/${zone-id}/records/${record-id}"
								:method              :put
								:basic-authorization (list username api-token)
								:accept              "application/json"
								:content-type        "application/json"
								:content             #?"{\"zone_record\": {\"data\": \"${(current-ip)}\"}}"))

(defun info-from-prompt ()
	(format *standard-output* "please enter needed information~%")
	(persist-info
	 (mapcar 'prompt-and-pair
					 '(":username"
						 ":api-token"
						 ":zone-id"
						 ":record-id"))))

(defun persist-info (info)
	(with-open-file (stream *info-file*
													:direction         :output
													:if-exists         :overwrite
													:if-does-not-exist :create)
		(format stream "(~{(~{~a ~s~})~^ ~})" info)))

(defun read-info ()
	(if (probe-file *info-file*)
			(with-open-file (stream *info-file*)
				(validate-info (read stream)))
			(info-from-prompt)))

(defun validate-info (info)
	(let* ((keys   '(:username :api-token :zone-id :record-id))
				 (length (length keys))
				 (result (mapcar 'validate-info-member info)))
		(if (and (eql length (length result))
						 (eql length (length (intersection keys result))))
				info
				(progn
					(format *standard-output* "incorrect info format....~%")
					(info-from-prompt)))))

(defun validate-info-member (info-member)
	(when (and (listp info-member)
						 (eql 2 (length info-member))
						 (symbolp (first info-member)))
		(first info-member)))

(apply #'sync-record (flatten (read-info)))
(ccl::quit)

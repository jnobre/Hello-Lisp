;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Procura e Planeamento 2016/2017
; Joao Artur Ventura Valerio Nobre Nº87914 - G041 
; 20-Queens problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :user)

;Nome: resolve-problema
;Argumentos: estado inicial da tabuleiro e a estrategia a seguir (a*, profundidade [depth-first])
;Return:  matriz [tabuleiro] com a solução do problema
(defun resolve-problema (initial-state strategy)
	(let ((initial-state-transformed (convert-board-to-queens-state initial-state))
		  (result-state nil)
		  (transformed-result nil))
		(setf result-state (procura (cria-problema initial-state-transformed 
													(list #'operator)
												   	:objectivo? #'objective? 
												   	:heuristica #'heuristic) 
												   	strategy))
		(setf result-state (first (last (nth (- (length result-state) 4) result-state))))
		(when (not (null result-state))
			  (setf transformed-result (convert-queens-state-to-board result-state)))
		transformed-result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Funcoes Auxiliares ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Nome: queens-state
;Descricao:
; number-placed: numero de rainhas colocadas no tabuleiro
; positions: array n posicoes para cada linha do tabuleiro. O conteudo representa 
;a qual coluna a rainha x foi colocada
; columns-state: array com n posicoes para cada coluna do tabuleiro. (t se tiver ocupado ou nul se estiver vazio) 
(defstruct queens-state positions columns-state number-placed)

;Nome: queens-state-isEmpty
;Argumentos: tamanho do tabuleiro
;Return: versao vazia do tabuleiro
(defun queens-state-isEmpty (size)
	(make-queens-state 	:number-placed 0
					 	:positions (make-array size)
					 	:columns-state (make-array size)))

;Nome: copy-queens-state
;Argumentos: estrutura queens-state  
;Return: faz uma copia do tabuleiro
(defun copy-queens-state (state)
	(make-queens-state 	:number-placed (queens-state-number-placed state)
					 	:positions (copy-array (queens-state-positions state))
					 	:columns-state (copy-array (queens-state-columns-state state))))

;Nome: put-queen!
;Argumentos: linha, coluna e estrutura onde a rainha sera alocadaca
(defun put-queen! (state row col)
		(setf (aref (queens-state-positions state) row) col)
		(setf (aref (queens-state-columns-state state) col) t)
		(incf (queens-state-number-placed state)))

;Nome: result-of-move
;Argumentos: linha, coluna e estrutura onde a rainha sera alocadaa
;Return: uma copia do tabuleiro com linha, coluna e estrutura onde a rainha sera alocadaca ocupado
(defun result-of-move (state row col)
	(let ((state-copy (copy-queens-state state)))
		(put-queen! state-copy row col)
		state-copy))

;Nome: free-row?
;Argumentos: estrutura queens-state e linha
;Return: t se linha nao contiver nenhuma rainha, caso contrario, nil
(defun free-row? (state row)
	(null (aref (queens-state-positions state) row)))

;Nome: free-column?
;Argumentos: estrutura queens-state e coluna
;Return: t se a coluna nao contiver nenhuma rainha, caso contrario, nil
(defun free-column? (state column)
	(null (aref (queens-state-columns-state state) column)))

;Name: free-diagonal?
;Arguments: queens-state structure, the row and the column of the position
;Return: t if there is no queen in the same diagonals. nil if not.
;Side-effects: None
(defun free-diagonal? (state row column)
	(let ((positions (queens-state-positions state)))
		(dotimes (r (array-dimension positions 0))
			(let ((c (aref positions r)))
				(when (and (not (null c))
						   (= (abs (- r row))
			 	 	     	  (abs (- c column)))
			 	 	  (return-from free-diagonal? nil))))))
		t)

;Nome: free?
;Return: t se a posicao em causa (r,c) nao criar nenhum conflito com outra rainha, caso contrario, nul.
(defun free? (state r c)
	(and (free-row? state r)
		 (free-column? state c)
		 (free-diagonal? state r c)))


;Nome: convert-board-to-queens-state
;Argumentos: matrix o tabuleiro descrevendo o tabuleiro
;Return: state que representa o estado do tabuleiro tabuleiro
(defun convert-board-to-queens-state(board-matrix)
	(let* ((size (array-dimension board-matrix 0))
		   (state (queens-state-isEmpty size)))
		(dotimes (row size)
			(dotimes (column size)
				(when (eq (aref board-matrix row column) t)
					(put-queen! state row column)
					(return))))
		state))

;^conversao oposta da funcao anterior
(defun convert-queens-state-to-board(queen-state)
	(let* ((positions (queens-state-positions queen-state))
		   (size (array-dimension positions 0))
		   (result-board-matrix (make-array (list size size))))
		(dotimes (r size)
			(setf (aref result-board-matrix r (aref positions r)) t))
		result-board-matrix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MANIPULACAO DE POSICOES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Name: create-position
;Argumentos: linha e coluna
;Return: o par [linha, coluna]
(defun create-position (row col)
	(cons row col))

;Name: generate-rotated-positions 
;Arguments: posicoes e tamanho do tabuleiro
;Return: uma lista que representa posicoes das rotacoes dada as posices e o tamanho do tabuleiro
;Side-effects: None
;
;Exemplo  (gen-rotatepositions (create-position 1 0) 3)
;
; 0 | 0 | 0          0 | 0 | 0           0 | 1 | 0          0 | 0 | 0
; 0 | 0 | 0    <=>   1 | 0 | 0     <=>   0 | 0 | 0    <=>   0 | 0 | 1
; 0 | 1 | 0          0 | 0 | 0           0 | 0 | 0          0 | 0 | 0
;
; return ((2 . 1) (1 . 0) (0 . 1) (1 . 2))
(defun generate-rotated-positions (position size)
	(labels ((rotate-position-left (position size)
		(let ((row (car position))
		  	  (col (cdr position)))
			(create-position (- (- size 1) col) row))))

	(let ((result (cons position (list)))
		  (rotated-pos position))
		(dotimes (n-rotations 3)
			(setf rotated-pos (rotate-position-left rotated-pos size))
			(setf result (cons rotated-pos result)))
		result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OPERADORES PARA O RESOLVE-PROBLEMA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Name: operator
;Arguments: estado do tabuleiro
;Return: lista de sucessores compativeis
(defun operator (state)
	(let ((size (array-dimension (queens-state-positions state) 0))
		  (sucessors (list))
		  (rotated-positions (list)))
		(dotimes (c size)
			(when (free-column? state c)
				  (dotimes (r size)
				  	(when (and (free-row? state r)
							   (free-diagonal? state r c)							   
							   (null (member (create-position r c) rotated-positions :test #'equal))) ;evitar a geracao de sucessores para o tabuleiro rodado
						  (progn 
						  		(setf rotated-positions (append rotated-positions (generate-rotated-positions (create-position r c) size)))
						 	 	(setf sucessors (append sucessors (list (result-of-move state r c)))))))				  
				  (return-from operator sucessors))))) ;tabuleiro esta completo por coluna, retorna apos a iteracao todas as linhas


;Nome: objective?
;Argumentos: estado do tabuleiro
;Return: t if the number of placed queens is equals to the size of the board. nil if not.
;Return: Se o número de rainhas colocados é igual ao tamanho do tabuleiro, caso contrario, nil.
(defun objective? (state)
	(= (queens-state-number-placed state) (array-dimension (queens-state-positions state) 0)))
		
;Nome: heuristic
;Argumentos: estado do tabuleiro
;Return: Numero inteiro do valor final da heuristica 
;Heuristica: dar mais peso às colunas que têm mais posicoes livres e menos peso, se a linha correspondente
;é o ultimo no tabuleiro o que significa que a oportunidade de encontrar uma solução é menos provavel. Portanto, no final 
;de cada linha, contamos o número de ocnflitos encontrado e multiplicamos pelo número de linhas-livre. O resultado final
; é o produto da soma ponderada dos conflitos para cada linha e o número de linhas-livre, o que representa uma
;medida global do tabuleiro. Um tabuleiro com linhas mais livre é melhor que o outro bordo com menos linhas livres.
(defun heuristic (state)
  (let* ((heuristic 0)
         (positions (queens-state-positions state))
         (size (array-dimension positions 0))
         (n-free-rows 0))
    (dotimes (r size)
    	(let ((n-conflits 0))
	      	(when (null (aref positions r))
	        	  (incf n-free-rows)
	          	(dotimes (c size)
	            	(when (not (free? state r c))
	                	  (incf n-conflits))))
      		(setf heuristic (+ heuristic (* n-free-rows n-conflits)))))
    (* heuristic n-free-rows)))


;Tests
;(time (resolve-problema (make-array '(20 20)) "a*"))
;Real time: 1556.8909 sec.
;Run time: 1553.653 sec.
;Space: 2645911008 Bytes
;GC: 1487, GC time: 12.20556 sec.
;#2A((NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL)
;    (NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL)
;    (NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL)
;    (T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL))
;
;
;(time (resolve-problema (make-array '(20 20)) "profundidade"))
;Real time: 318.99258 sec.
;Run time: 317.30823 sec.
;Space: 436063432 Bytes
;GC: 189, GC time: 2.130942 sec.
;#2A((T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T)
;    (NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL)
;    (NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL)
;    (NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))




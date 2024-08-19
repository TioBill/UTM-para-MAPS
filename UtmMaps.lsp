;; Os codigos fontes foram tirados da IBM, e adaptador por mim para funcionar no AutoLisp

(defun tan ( x )
  (/ (sin x) (cos x))
)

(defun utm-to-geo (zone easting northing northenHemis / 
                   semi_eixo e elinquad k0 arc mu ei ca cb cc cd phi1 n0 
                   r0 fact1 _a1 dd0 fact2 t0 Q0 fact3
                   fact4 lof1 lof2 lof3 _a2 _a3
                   )
  (if (not northenHemis)
    (setq northing (- 10000000 northing))
  )
  
  (setq
    semi_eixo 6378137
    achat 3.35281068e-03
    e (sqrt (- (* 2.0 achat) (expt achat 2.0)))
    elinquad (/ (expt e 2) (- 1 (expt e 2)))
    
    
    k0 0.9996
    arc (/ northing k0)
    mu (/ arc (* semi_eixo (- 1 (/ (expt e 2) 4.0) (* 3 (/ (expt e 4) 64.0)) (* 5 (/ (expt e 6) 256.0)))))
    ei (/ (- 1 (expt (- 1 (expt e 2)) (/ 1 2.0))) (+ 1 (expt (- 1 (expt e 2)) (/ 1 2.0))))
    ca (- (* 3 (/ ei 2)) (* 27 (/ (expt ei 3) 32.0)))
    cb (- (/ (* 21 (expt ei 2)) 16) (* 55 (/ (expt ei 4) 32)))
    cc (* 151 (/ (expt ei 3) 96))
    cd (* 1097 (/ (expt ei 4) 512))
    phi1 (+ mu (* ca (sin (* 2 mu))) (* cb (sin (* 4 mu))) (* cc (sin (* 6 mu))) (* cd (sin (* 8 mu))))
    n0 (/ semi_eixo (expt (- 1 (expt (* e (sin phi1)) 2)) (/ 1 2.0)))
    r0 (/ (* semi_eixo (- 1 (* e e))) (expt (- 1 (expt (* e (sin phi1)) 2)) (/ 3 2.0)))
    fact1 (* n0 (/ (tan phi1) r0))
    _a1 (- 500000 easting)
    dd0 (/ _a1 (* n0 k0))
    fact2 (/ (* dd0 dd0) 2)
    t0 (expt (tan phi1) 2)
    Q0 (* elinquad (expt (cos phi1) 2))
    fact3 (* (- (- (+ (+ 5 (* 3 t0)) (* 10 Q0)) (* 4 (* Q0 Q0))) (* 9 elinquad)) (/ (expt dd0 4) 24))
    fact4 (* (- (+ (+ 61 (* 90 t0)) (* 298 Q0) (* 45 t0 t0)) (* 252 elinquad) (* 3 Q0 Q0)) (/ (expt dd0 6) 720))
    lof1 (/ _a1 (* n0 k0))
    lof2 (* (+ (* 2 t0) 1 Q0) (/ (expt dd0 3) 6.0))
    lof3 (* (+ (- (+ (- 5 (* 2 Q0)) (* 28 t0)) (* 3 (expt Q0 2))) (* 8 elinquad) (* 24 (expt t0 2))) (/ (expt dd0 5) 120))
    _a2 (/ (+ (- lof1 lof2) lof3) (cos phi1))
    _a3 (/ (* _a2 180) pi)
    latitude (/ (* (- phi1 (* fact1 (+ fact2 fact3 fact4))) 180) pi)
  )
  
  (if (not northenHemis)
    (setq latitude (* latitude -1))
  )
  
  (if (> zone 0)
    (setq longitude (- (* 6 zone) 183.0 _a3))
    (setq longitude (- 3.0 _a3))
  )

  (list latitude longitude)
)

(defun c:utmMaps_help ()
  (princ "\n========\n")
  (princ "O programa, leva em consideracao o ponto X, Y.\nEm um sistema UTM em SIRGAS2000 e converte para o WGS84.\n")
  (princ "A zona levada em consideracao eh de numero Z: 22.\nE tendo O Hemisferio da coordenada Y como SUL\n")
  (princ "DISCLAIMER: Provavelmente no futuro, esse programa não conseguira mantar as coordenadas para o\n")
  (princ "GOOGLE MAPS, por questoes diversas. Nesse caso. Digite em seu console no seu CAD o coando\n")
  (princ "utmMapsGet  Nela, tera a latitude e longitude, nessa ordem. Copie-las e pesquise no Google Maps\n")
  (princ "No seguinte padrao: 123.45, -22.546\n")
  (princ "\n========\n")
  (princ)
)

(defun c:utmMapsGet ()
  (princ "\n========\n")
  (if (and latitude longitude)
    (progn
      (princ (strcat "LATITUDE: " (rtos latitude 2 12))) 
      (terpri)
      (princ (strcat "LONGITUDE: " (rtos longitude 2 12))) 
    )
    
    (princ "Por favor! Execute primeiro o comando utmMaps")
  )
  (princ "\n========\n")
  
  (princ)
)

(defun c:utmMaps ( / lista hemis zone lista)

  (setq lista (getpoint "Selecione o ponto desejado: "))
  
  (setq 
    hemis nil
    zone 22.0
  ) 
    
  (setq lista (utm-to-geo zone (car lista) (cadr lista) hemis))

  (command "_browser"
    (strcat "http://maps.google.com.br/maps?q=" (rtos (car lista) 2 12) "," (rtos (cadr lista) 2 12 ) "&hl=pt-BR&t=h&z=16")
  )
  
  (princ)
)

(alert "Digite \"utmMaps\" para comecar! Caso tenha alguma duvida, digite \"utmMaps_help\"")


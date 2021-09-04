
(set-logic ALL)

(declare-const ur_0 Real)
(declare-const ur_2 Real)
(declare-const ur_4 Real)
(declare-const ub_1 Bool)


(assert (exists ((reqv0 Real) (beqv1 Bool)) (< (ite (let ((bli_b_1022 beqv1)) true) ur_0 (ite ub_1 (- 732303849024073567780296577407829463971809942686400512.000000) ur_4)) (* (ite ub_1 37597257429187968513974770412226386349766213632.000000 reqv0) ur_2))))

(declare-fun ufb_2 (Int Real) Bool)
(declare-const ur_1 Real)


(assert (= (xor (forall ((buqv2 Bool)) (or false buqv2)) (exists ((reqv0 Real)) (ufb_2 (- 3530095089560171089) reqv0))) (let ((bli_b_1692 (exists ((beqv3 Bool)) (= beqv3 true)))) (distinct ur_0 ur_1)))) (check-sat)



(assert (exists ((beqv3 Bool)) beqv3)) (check-sat)

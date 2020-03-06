"seat.calculator" <-
    function(
             ## 비례대표 국회의원 선거 득표비율
             pr.vote.share = c(25.54, 33.5, 26.79, 14.17, 0,0,0,0,0)/100,
             ## 지역구 의석수
             smd.seat = c(110, 105, 25, 2, 0,0,0,0,0),
             nonparty.district.winner = 11
             ){
        total.member = 300
        n.party <- length(pr.vote.share)
        if(n.party!=length(smd.seat))
           stop("\n비례득표율과 지역구의석수의 정당수가 다릅니다! \n")

        ## 지역구 총의석수 확인
        if(253 != sum(smd.seat) + nonparty.district.winner)
            stop("\n지역구 의석수가 253이 아닙니다!\n")

        ## 준연동비례계산
        semiyeondong <- rep(NA, n.party)
        for(i in 1:n.party){
            semiyeondong[i] <- ((total.member -
                                 nonparty.district.winner)*pr.vote.share[i] -
                                smd.seat[i]) * 0.5
        }
        ## 반올림!
        semiyeondong.out <- ifelse(semiyeondong < 0, 0, round(semiyeondong))
        semiyeondong.pr.seat <- round(30*semiyeondong.out/sum(semiyeondong.out))
        ## 병립형비례계산
        parallel.pr.seat <- round(pr.vote.share*17)

        outcome = list(
            total.seat = smd.seat + semiyeondong.pr.seat + parallel.pr.seat,
            smd.seat = smd.seat,
            semiyeondong.pr.seat = semiyeondong.pr.seat,
            parallel.pr.seat =  parallel.pr.seat)
        return(outcome)

    }

## written by JHP 2020-03-08
## revised by Hoi Sung Jeong 2020-03-09

"seat.calculator" <-
  function(
    ## 비례대표 국회의원 선거 득표비율 
    pr.vote.share = c(40.40, 32.10, 4.10, 3.80, 4.40, 1.7, 1.1, 1.0, 1.5, 2, 2, 2, 2, 1.9)/100 ,
    ## 지역구 의석수
    smd.seat = c(116, 91, 7, 8, 2, 2, 3, 1, 3, 0, 0, 0, 0, 0),
    
    ## 무소속 지역구 당선자
    nonparty.district.winner = 20
    )
  {
    total.member = 300
    n.party <- length(pr.vote.share)
    if(n.party!=length(smd.seat))
        stop("\n비례득표율과 지역구의석수의 정당수가 다릅니다! \n")
    if(sum(pr.vote.share)!=1)
        stop("\npr.vote.share의 합은 1이 되어야 합니다! \n")
    ## 지역구 총의석수 확인
    if(253 != sum(smd.seat) + nonparty.district.winner)
        stop("\n지역구 의석수가 253이 아닙니다!\n")
    
    ## 1. 의석할당정당 및 연동배분의석수 확인
    haldang.pr.vote.share <- ifelse( pr.vote.share < 0.03 & smd.seat < 5, 0, pr.vote.share )
    nonhaldang.smd.seat <- ifelse( pr.vote.share < 0.03 & smd.seat < 5, smd.seat, 0)
    party.earned <- haldang.pr.vote.share/sum(haldang.pr.vote.share)
    yeondong <- 300 - sum(nonhaldang.smd.seat) - nonparty.district.winner
    
    ## 2. 연동형 캡 30석 계산: 잔여 의석 혹은 초과 의석 처리
    basic.formula <- ifelse( pr.vote.share < 0.03 & smd.seat < 5,
                            0, 
                            (yeondong*party.earned - smd.seat)*0.5 )
    hwansan <- ifelse(basic.formula < 1, 0,round(basic.formula))
    ifelse(sum(hwansan) > 30, 
           resid <- 30 * basic.formula/sum(hwansan),
           resid <- (30 - sum(hwansan))*party.earned)
    resid1 <- ifelse(resid < 1, 0, resid)
    resid2 <- floor(resid1)
    ifelse(sum(hwansan)>30, 
           left <- 30 - sum(resid2), 
           left <- 30 - sum(hwansan) - sum(resid2) )
    digit.big.order <- sort(resid1-resid2, decreasing = T, index.return=TRUE)$ix 
    resid3 <- rep(0, length(digit.big.order))
    for(i in 1:left){
        resid3[digit.big.order[i]] <- 1
    }
    ifelse(sum(hwansan)>30, 
           yeondongcap <- resid2 + resid3, 
           yeondongcap <- hwansan + resid2 + resid3)
    
    ## 3. 병립형 17석 계산
    byunglib <- 17*party.earned
    byunglib2 <- floor(byunglib)
    bl.order <- sort(byunglib-byunglib2, decreasing = T, index.return=TRUE)$ix 
    left2 <- 17 - sum(byunglib2)
    byunglib3 <- rep(0, length(bl.order))
    for(i in 1:left2){
        byunglib3[bl.order[i]] <- 1
    }
    byunglibcap <- byunglib2+ byunglib3
    
    ## 결과 표시 
    outcome = list(
        total.seat = smd.seat + yeondongcap + byunglibcap,
        smd.seat = smd.seat,
        semiyeondong.pr.seat = yeondongcap,
        parallel.pr.seat = byunglibcap,
        partydist.nonpartydist.yeondong.byunglib = c(sum(smd.seat), nonparty.district.winner, 
                                                     sum(yeondongcap), sum(byunglibcap))
      # for check 
      # party.earned = party.earned,
      # yeondong.split = yeondong,
      # basic.formula = basic.formula,
      # hwansan = hwansan,
      # resid = resid,
      # resid1 = resid1,
      # resid2 = resid2,
      # digit = resid1-resid2,
      # howmanyleft = left,
      # resid3 = resid3,
      # byunglib = byunglib,
      # byunglib2 = byunglib2,
      # digitbl = byunglib-byunglib2,
      # howmanyleftbl = left2,
      # byunglib3 = byunglib3
      
      )
    return(outcome)
    
  }

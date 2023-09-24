      program   ladcp3
*------------------------------------------------
* Wの残差によるクライテリオン
* W残差の側線平均と標準偏差をだすプログラム
* Last update 2004/5/7
*20050610 BIN2 dataはおかしい可能性がありそれを除いてクライテリオンを決める
*20050610手直し中！！
*20060207手直しはやめる 標準偏差4倍->3倍
*-------------------------------------------------
      implicit  none
      integer   nb(0:5),n,nn,k,nko,na(50),nterm,nsta
      real      a(50),dmax,xsig,xsum,
     &          xhei1,xhei2,xhei3,xhen1,xhen2,xhen3
      character sta*3,UPDW*2
*
* 調べるデータのディレクトリ設定(DW,UP,BT)
      UPDW='dw'


* 側線の測点設定
      nb(0)=0
      nb(1)=1

* ヒストグラムファイル
c      open(20,file=
c     &'./DAT/LADCP3/WZAMEMO.'//UPDW//'',
c     &    form='formatted')
*
* 計算開始
      do n=1,1
*
*****1回目の標準偏差を求める
* 初期設定
      nterm=1
      xsig=0.0
      nko=0
      xsum=0.0
      do k=1,50
      na(k)=0.0
      end do
      do k=1,50
      a(k)=-49.0+2.0*(k-1.0)
      end do

      do nn=nb(n-1)+1,nb(n)
*    *STATION 名読み出し+W残差の読み込み
*    *1回目の平均
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 100
      end if
      CALL RDFILE1(nterm,UPDW,sta,nko,xsum,xhei1,xhen1,na,a)
      write(*,*)nterm,'  ',sta,nko,xsum
 100  continue
      end do
      xhei1=xsum/nko
      write(*,*)xhei1,'  = 1回目の平均'

      do nn=nb(n-1)+1,nb(n)
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 200
      end if
      CALL RDFILE2(nterm,UPDW,sta,xhei1,xsig,nko,xhen1)
      write(*,*)nterm,'  ',sta,xhei1,xsig,xhen1
 200  continue
      end do
*    *1回目の標準偏差
      xhen1=sqrt(xsig/(nko-1))   
     
      write(*,*)xhen1,'  (',xsig,')  ','= 1回目の標準偏差 '
      write(20,*)n,' 測線番号（1回目）'
      do k=1,49
      write(20,*)a(k)+1.0,na(k)
      end do
      write(20,*)xhei1,xhen1

******2回目の標準偏差を求める
* 初期設定
      nterm=2
      xsig=0.0
      nko=0
      xsum=0.0
      do k=1,50
      na(k)=0.0
      end do

      do nn=nb(n-1)+1,nb(n)
*    *STATION 名読み出し+EVの読み込み
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 300
      end if
      CALL RDFILE1(nterm,UPDW,sta,nko,xsum,xhei1,xhen1,na,a)
      write(*,*)nterm,'  ',sta,nko,xsum
 300  continue
      end do
      xhei2=xsum/nko
      write(*,*)xhei2,' = 2回目の平均 '

      do nn=nb(n-1)+1,nb(n)
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 400
      end if
      CALL RDFILE2(nterm,UPDW,sta,xhei2,xsig,nko,xhen1)
      write(*,*)nterm,'  ',sta,xhei2,xsig,xhen1
 400  continue
      end do
*    *2回目の標準偏差
      xhen2=sqrt(xsig/(nko-1))   
     
      write(*,*)xhen2,xsig,' = 2回目 標準偏差'
      write(20,*)n,'   2回目'
      do k=1,49
      write(20,*)a(k)+1.0,na(k)
      end do
      write(20,*)xhei2,xhen2

*****3回目の標準偏差を求める
* 初期設定
      nterm=3
      xsig=0.0
      nko=0
      xsum=0.0
      do k=1,50
      na(k)=0.0
      end do

      do nn=nb(n-1)+1,nb(n)
*    *STATION 名読み出し+EVの読み込み
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 500
      end if
      CALL RDFILE1(nterm,UPDW,sta,nko,xsum,xhei2,xhen2,na,a)
      write(*,*)nterm,'  ',sta,xhei2,xsig,xhen2
 500  continue
      end do
      write(*,*)xsum,nko
      xhei3=xsum/nko
      write(*,*)xhei3,' = 3回目の平均 '

      do nn=nb(n-1)+1,nb(n)
      CALL READST(nn,UPDW,sta,dmax,nsta)
      if(nsta.EQ.1)then
        goto 600
      end if
      CALL RDFILE2(nterm,UPDW,sta,xhei3,xsig,nko,xhen2)
      write(*,*)nterm,'  ',sta,xhei3,xsig,xhen2
 600  continue
      end do

*    *3回目の標準偏差
      xhen3=sqrt(xsig/(nko-1))   
      write(20,*)xhei3,xhen3
      write(*,*)xhei3,xhen3,' = 3回目の平均、標準偏差'
*
      write(*,*)'側線   ',n,' 終了!'
      end do

      STOP
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE READST(nn,UPDW,sta,dmax,nsta)
      CHARACTER  sta*3,UPDW*2
**  ステーション名,月,緯度,経度を読み込むプログラム
      open(15,file='./staname',
     &   form='formatted',status='old')
*初期設定
      sta='/   /'
      nsta=0

c 1    format(a3,3x,i1,3x,i2,x,f5.2,3x,i3,x,f5.2)
      if(nn.EQ.1)then
        goto 50
      end if
      do n=1,nn-1
      read(15,*,end=100)
      end do
 50   read(15,*,end=100)sta,month,n1,x1,n2,x2
      xlat=n1+x1/60.0
      xlon=n2+x2/60.0
      write(*,*)'station C',sta,
     &'    緯度',xlat,'   経度',xlon
 100  continue
      close(15)
* データが無い測点

      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE RDFILE1(nterm,UPDW,sta,
     &nko,xsum,xhei0,xhen,na,a)
      REAL*8     xt0,xt
      CHARACTER  sta*3,UPDW*2
      DIMENSION  na(50),a(50),xw(41)
**  LADCP2/UP/ DATAを読み込むプログラム
**  xt0,xtは少数点以下5桁まであるので、REAL指定
**  アンサンブル平均を求める
      open(10,file=
     &'./DAT/LADCP2/'//sta//'.'//UPDW//'',
     &    form='formatted',status='old')
* 初期設定 a(k):ヒストグラム xt0:タイムxtの初期値
      do k=1,50
      a(k)=-49.0+2.0*(k-1.0)
      end do
**
      read(10,*)xn0,xt0,xm,xi,xm,xm,xw(1),xm,xm,xm,xm,xm,xm
      do x=1,1000000
      do j=2,40
      read(10,*,end=100)xn,xt,xm,xi,xm,xm,xw(j),xm,xm,xm,xm,xm,xm

        if(xn.NE.xn0)then
           goto 50
        else
           goto 90
        end if

*term=1で使用するアンサンブル平均値xheiを求める
 50   xsum0=0.0
      nko0=0
      do i=1,j-1
      if(abs(xw(i)).LT.9990.0)then
      xsum0=xsum0+xw(i)
      nko0=nko0+1
      end if
      end do
      xhei1=xsum0/nko0
*   term=2 で使用する平均値xheiを求める
      xsum0=0.0
      nko0=0
      do i=1,j-1
      if((abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei1)-xhei0).LT.4*xhen))then
      xsum0=xsum0+xw(i)
      nko0=nko0+1
      end if
      end do
      xhei2=xsum0/nko0
*   term=3 で使用する平均値xheiを求める
      xsum0=0.0
      nko0=0
      do i=1,j-1
      if((abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei2)-xhei0).LT.3*xhen))then
      xsum0=xsum0+xw(i)
      nko0=nko0+1
      end if
      end do
      xhei3=xsum0/nko0

* 残差を求める
      do i=1,j-1
*   term＝1 には、すべてのデータから残差を求める
      if((nterm.EQ.1).AND.(abs(xw(i)).LT.9990.0))then
*       * アンサンブル平均xheiからの残差
        xsum=xsum+(xw(i)-xhei1)
        nko=nko+1
      end if

*       * 残差のヒストグラム
        do k=1,49
        if((nterm.EQ.1).AND.(abs(xw(i)).LT.9990.0).AND.
     &((xw(i)-xhei1).LT.a(k+1)).AND.((xw(i)-xhei1).GE.a(k))
     &)then
        na(k)=na(k)+1
        end if
        end do


*   term＝2 には、標準偏差の3倍を除いて残差を求める
      if((nterm.EQ.2).AND.(abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei1)-xhei0).LT.4*xhen))then
        xsum=xsum+(xw(i)-xhei2)
        nko=nko+1
      end if

*       * 残差のヒストグラム
          do k=1,49
          if((nterm.EQ.2).AND.(abs(xw(i)).LT.9990.0)
     &.AND.((abs(xw(i)-xhei1)-xhei0).LT.4*xhen).AND.
     &((xw(i)-xhei2).LT.a(k+1)).AND.((xw(i)-xhei2).GE.a(k))
     &)then
          na(k)=na(k)+1
          end if
          end do


*   term＝3 には、標準偏差の1倍を除いて残差を求める
      if((nterm.EQ.3).AND.(abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei2)-xhei0).LT.3*xhen))then
        xsum=xsum+(xw(i)-xhei3)
        nko=nko+1
      end if
      end do

*   次のアンサンブルのための設定
           xn0=xn
           xw(1)=xw(j)
           goto 95

 90   end do

 95   end do
 100  continue
      close(10)

      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *
*   *   *   *   *   *   *   *   *   *   *   *   *   *
      SUBROUTINE RDFILE2(nterm,UPDW,sta,xhei0,xsig,nko,xhen)
      REAL*8     xt
      CHARACTER  sta*3,UPDW*2
      DIMENSION  xw(41),xt(41),xn(41),x1(41),x2(41),x3(41),x4(41),
     &x5(41),x6(41),x7(41),x8(41),x9(41),x10(41)
**  LADCP2/UP/ DATAを読み込むプログラム
**  アンサンブル平均を求める
      open(10,file=
     &'./DAT/LADCP2/'//sta//'.'//UPDW//'',
     &    form='formatted',status='old')
      if(nterm.EQ.3)then
      open(30,file=
     &'./DAT/LADCP3/'//sta//'.'//UPDW//'',
     &    form='formatted')
      end if
* 初期設定 タイムxtの初期値
 1    format(f7.1,f12.5,2x,f7.1,2x,f4.1,2x,9(f8.2,x))
      read(10,*)xn(1),xt(1),x1(1),x2(1),x3(1),x4(1),xw(1),
     &x5(1),x6(1),x7(1),x8(1),x9(1),x10(1)

      do x=1,1000000
      do j=2,40
      read(10,*,end=100)xn(j),xt(j),x1(j),x2(j),x3(j),x4(j),
     &xw(j),x5(j),x6(j),x7(j),x8(j),x9(j),x10(j)
        if(xn(j).NE.xn(1))then
           goto 50
        else
           goto 90
        end if

*term=1で使用するアンサンブル平均値xheiを求める
 50   xsum0=0.0
      nko0=0
      do i=1,j-1
      if(abs(xw(i)).LT.9990.0)then
      xsum0=xsum0+xw(i)
      nko0=nko0+1
      end if
      end do
      xhei1=xsum0/nko0



*   term=2 で使用する平均値xheiを求める
      xsum0=0.0
      nko0=0
      do i=1,j-1
      if((abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei1)-xhei0).LT.4*xhen))then
      xsum0=xsum0+xw(i)
      nko0=nko0+1
      end if
      end do
      xhei2=xsum0/nko0


*   term=3 で使用する平均値xheiを求める
      xsum0=0.0
      nko0=0
      do i=1,j-1
      if((abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei2)-xhei0).LT.3*xhen))then
      xsum0=xsum0+xw(i)
      nko0=nko0+1
      end if
      end do
      xhei3=xsum0/nko0

* 残差を求める
      do i=1,j-1
*   term＝1 には、すべてのデータから残差を求める
      if((nterm.EQ.1).AND.(abs(xw(i)).LT.9990.0))then
*       * アンサンブル平均xheiからの残差
        xsig=xsig+((xw(i)-xhei1)-xhei0)**2
        nko=nko+1
      end if



*   term＝2 には、標準偏差の倍を除いて残差を求める
      if((nterm.EQ.2).AND.(abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei1)-xhei0).LT.4*xhen))then
        xsig=xsig+((xw(i)-xhei2)-xhei0)**2
        nko=nko+1
      end if




*   term＝3 には、標準偏差の倍を除いて残差を求める
      if((nterm.EQ.3).AND.(abs(xw(i)).LT.9990.0).AND.
     &(abs((xw(i)-xhei2)-xhei0).LT.3*xhen))then
        xsig=xsig+((xw(i)-xhei3)-xhei0)**2
        nko=nko+1
        write(30,1)xn(i),xt(i),x1(i),x2(i),x3(i),x4(i),
     &xw(i),x5(i),x6(i),x7(i),x8(i),x9(i),x10(i)
      end if
      end do

*   次のアンサンブルのための設定
           xn(1)=xn(j)
           xt(1)=xt(j)
           xw(1)=xw(j)
           x1(1)=x1(j)
           x2(1)=x2(j)
           x3(1)=x3(j)
           x4(1)=x4(j)
           x5(1)=x5(j)
           x6(1)=x6(j)
           x7(1)=x7(j)
           x8(1)=x8(j)
           x9(1)=x9(j)
           x10(1)=x10(j)
           goto 95

 90   end do

 95   end do
 100  continue
      close(10)

      RETURN
      END
*   *   *   *   *   *   *   *   *   *   *   *   *   *

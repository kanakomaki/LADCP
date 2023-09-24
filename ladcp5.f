      program   ladcp5
* Shear をビン、深さごとに平均を出して、平均との差が大きすぎるものを省く ++

      implicit  none
      integer   nn,n,i,j,x,numi(301,40),numiu2(301,40),numiv2(301,40)
     &,k,umink(301),vmink(301),kou,kov,numiu3(301,40),numiv3(301,40)
      real      z,u,v,p,binkan,m,d(301),
     &          sumui(301,40),sumvi(301,40),
     &          sau1(301,40),sav1(301,40),sau2(301,40),sav2(301,40),
     &          a(51),numa(301,40,51),
     &          sumui2(301,40),sumvi2(301,40),
     &          sumui3(301,40),sumvi3(301,40),
     &          sau3(301,40),sav3(301,40),numu(301,40),numv(301,40),
     &          sau3min,sav3min,
     &          heikinu,heikinv,hu,hv
      character sta*2

      open(5,file='/home/ocg/kanae/LEG2/stanamekh03',
     &   form='formatted',status='old')

 1    format(a2)
 2    format(2(x,f15.4),x,i10)
 3    format(19(x,f5.0))
**はじめの設定 下
c      do x=1,50
c      d(x)=20.0*x
c      end do
c      do x=51,180
c      d(x)=50.0*(x-50)+1000.0
c      end do
** 変えた設定
      do x=1,50
      d(x)=20.0*x
      end do
      do x=51,301
      d(x)=50.0*(x-50)+d(50)
      end do
******ヒストグラムの設定
      do j=1,42
      a(j)=-4.1+0.2*(j-1)
      end do
******************************
      do nn=1,29
      read(5,*)
      end do
      do nn=1,29
      read(5,1,end=1000)sta
      write(*,1)sta
**
      open(10,file='/home/ocg/kanae/LEG2/C'//sta//'T/shup',
     &               form='formatted',status='old')
      open(20,file='/home/ocg/kanae/LEG2/C'//sta//'T/shup2050',
     &               form='formatted',status='new')
      open(40,file='/home/ocg/kanae/LEG2/C'//sta//'T/sh2up2050',
     &               form='formatted',status='new')
      open(25,file='/home/ocg/kanae/LEG2/C'//sta//'T/shup2050^memo',
     &               form='formatted',status='new')
c      open(30,file=
c     &'/home/ocg/kanae/LEG2/C'//sta//'T/shup2050^s^allav',
c     &               form='formatted',status='new')
*++ビンごとに平均を出して一番良いものをシアとした
      open(35,file='/home/ocg/kanae/LEG2//C'//sta//'T/shup2050^s',
     &               form='formatted',status='new')
*++ビンごとの平均の平均
      do x=1,301
       do i=1,40
       sumui(x,i)=0.0
       sumvi(x,i)=0.0
       numi(x,i)=0
       sumui2(x,i)=0.0
       sumvi2(x,i)=0.0
       numiu2(x,i)=0
       numiv2(x,i)=0
       sumui3(x,i)=0.0
       sumvi3(x,i)=0.0
       numiu3(x,i)=0
       numiv3(x,i)=0
       sau1(x,i)=0
       sav1(x,i)=0
       sau2(x,i)=0
       sav2(x,i)=0
       do j=1,51
       numa(x,i,j)=0
       end do
       end do
      end do
** ビンごとにシアの平均を求める
      do n=1,2000000
      read(10,*,end=100)z,u,v,p,binkan,m,m
      
      do x=1,300

      do i=1,40
      if((z.GT.d(x)).AND.(z.LE.d(x+1)).AND.(binkan.EQ.i))then
      sumui(x,i)=sumui(x,i)+u
      sumvi(x,i)=sumvi(x,i)+v
      numi(x,i)=numi(x,i)+1
      end if
      end do

      end do   
      end do
 100  continue
      write(*,*)'平均読み出し 終了'
      write(*,*)'x=10 1bin 平均 =',sumui(10,1)/numi(10,1)
      write(*,*)'x=10 4bin 平均 =',sumui(10,4)/numi(10,4)
      close(10)
**** 標準偏差を求める
      open(10,file='/home/ocg/kanae/LEG2/C'//sta//'T/shup',
     &               form='formatted',status='old')
*****************************************************
      do n=1,2000000
      read(10,*,end=200)z,u,v,p,binkan,m,m

      do x=1,300
       if((z.GT.d(x)).AND.(z.LE.d(x+1)))then
         x=x
         goto 190
       end if
      end do
 190  i=binkan

      if((z.GT.d(x)).AND.(z.LE.d(x+1))
     & .AND.(binkan.EQ.i).AND.(numi(x,i).NE.0))then
      sau1(x,i)=(u-(sumui(x,i)/numi(x,i)))**2+sau1(x,i) 
      sav1(x,i)=(v-(sumvi(x,i)/numi(x,i)))**2+sav1(x,i)
      end if

      end do
 200  continue

      do x=1,300
      do i=1,40
       if(numi(x,i).GE.2)then
       sau1(x,i)=sqrt(sau1(x,i)/(numi(x,i)-1))
       sav1(x,i)=sqrt(sav1(x,i)/(numi(x,i)-1))

       else
       sau1(x,i)=9999
       sav1(x,i)=9999
       end if
      end do
      end do
      write(*,*)'標準偏差 終了'
      close(10)
**** 標準偏差の2倍で切り落とす
      open(10,file='/home/ocg/kanae/LEG2/C'//sta//'T/shup',
     &               form='formatted',status='old')
****************************************************
 250  format(3(x,f8.2),2(x,f8.0))
      do n=1,2000000
      read(10,*,end=300)z,u,v,p,binkan,m,m

      do x=1,300
        if((z.GT.d(x)).AND.(z.LE.d(x+1)))then
        x=x
        goto 290
        end if
      end do
 290  continue
      i=binkan

      if((z.GT.d(x)).AND.(z.LE.d(x+1))
     &.AND.(binkan.EQ.i).AND.(numi(x,i).GE.2)
     &.AND.(sau1(x,i).LT.1.0).AND.(sav1(x,i).LT.1.0) )then
       hu=8
       hv=8
      elseif((z.GT.d(x)).AND.(z.LE.d(x+1))
     &.AND.(binkan.EQ.i).AND.(numi(x,i).GE.2)
     &.AND.(sau1(x,i).GT.1.0).AND.(sav1(x,i).LT.1.0) )then
       hu=2
       hv=8
      elseif((z.GT.d(x)).AND.(z.LE.d(x+1))
     &.AND.(binkan.EQ.i).AND.(numi(x,i).GE.2)
     &.AND.(sau1(x,i).LT.1.0).AND.(sav1(x,i).GT.1.0) )then
       hu=8
       hv=2
      else
       hu=2
       hv=2
      end if

      if((z.GT.d(x)).AND.(z.LE.d(x+1))
     &.AND.(binkan.EQ.i).AND.(numi(x,i).GE.2)
     &.AND.(abs(u-sumui(x,i)/numi(x,i)).LE.sau1(x,i)*hu)
     &.AND.(abs(v-sumvi(x,i)/numi(x,i)).LE.sav1(x,i)*hv) )then
      write(20,250)z,u,v,p,binkan
      sumui2(x,i)=sumui2(x,i)+u
      sumvi2(x,i)=sumvi2(x,i)+v
      numiu2(x,i)=numiu2(x,i)+1
      numiv2(x,i)=numiv2(x,i)+1

      elseif((z.GT.d(x)).AND.(z.LE.d(x+1))
     &.AND.(binkan.EQ.i).AND.(numi(x,i).GE.2)
     &.AND.(abs(u-sumui(x,i)/numi(x,i)).LE.sau1(x,i)*hu)
     &.AND.(abs(v-sumvi(x,i)/numi(x,i)).GT.sav1(x,i)*hv) )then
      write(20,250)z,u,9999.0,p,binkan
      sumui2(x,i)=sumui2(x,i)+u
      numiu2(x,i)=numiu2(x,i)+1

      elseif((z.GT.d(x)).AND.(z.LE.d(x+1))
     &.AND.(binkan.EQ.i).AND.(numi(x,i).GE.2)
     &.AND.(abs(u-sumui(x,i)/numi(x,i)).GT.sau1(x,i)*hu)
     &.AND.(abs(v-sumvi(x,i)/numi(x,i)).LE.sav1(x,i)*hv) )then
      write(20,250)z,9999.0,v,p,binkan
      sumvi2(x,i)=sumvi2(x,i)+v
      numiv2(x,i)=numiv2(x,i)+1 
      end if

      end do
 300  continue
      close(20)
**** 2回目の標準偏差を求める *************************
      open(20,file='/home/ocg/kanae/LEG2/C'//sta//'T/shup2050',
     &               form='formatted',status='old')
*******************************************************
      do n=1,2000000
      read(20,*,end=400)z,u,v,p,binkan

      do x=1,300
      if((z.GT.d(x)).AND.(z.LE.d(x+1)))then
      x=x
      goto 390
      end if
      end do
 390  i=binkan

      if((z.GT.d(x)).AND.(z.LE.d(x+1)).AND.(u.NE.9999.0)
     & .AND.(binkan.EQ.i).AND.(numiu2(x,i).NE.0))then
      sau2(x,i)=(u-(sumui2(x,i)/numiu2(x,i)))**2+sau2(x,i)  
      end if
      if((z.GT.d(x)).AND.(z.LE.d(x+1)).AND.(v.NE.9999.0)
     & .AND.(binkan.EQ.i).AND.(numiv2(x,i).NE.0))then
      sav2(x,i)=(v-(sumvi2(x,i)/numiv2(x,i)))**2+sav2(x,i)
      end if
** ヒストグラム 作成 **
      do j=1,41
      if((z.GT.d(x)).AND.(z.LE.d(x+1)).AND.(u.NE.9999.0)
     &.AND.(binkan.EQ.i).AND.(numiu2(x,i).NE.0)
     &.AND.((u-(sumui2(x,i)/numiu2(x,i))).GT.a(j)).AND.
     &((u-(sumui2(x,i)/numiu2(x,i))).LE.a(j+1)))then      
      numa(x,i,j)=1+numa(x,i,j)
      end if
      end do

      end do
 400  continue

      do x=1,300
      do i=1,40
       if(numiu2(x,i).GE.2)then
       sau2(x,i)=sqrt(sau2(x,i)/(numiu2(x,i)-1))
       end if
       if(numiv2(x,i).GE.2)then
       sav2(x,i)=sqrt(sav2(x,i)/(numiv2(x,i)-1))
       end if

       if((numiu2(x,i).GE.2))then

       end if

      end do
      end do
      write(*,*)'2回目 標準偏差 終了'
      close(20)


**** 2回目の標準偏差の2倍で切り落とす
      open(10,file='/home/ocg/kanae/LEG2/C'//sta//'T/shup2050',
     &               form='formatted',status='old')
****************************************************
      do n=1,2000000
      read(10,*,end=500)z,u,v,p,binkan,m,m

      do x=1,300
      if((z.GT.d(x)).AND.(z.LE.d(x+1)))then
      x=x
      goto 590
      end if
      end do
 590  i=binkan

      if((z.GT.d(x)).AND.(z.LE.d(x+1))
     &.AND.(binkan.EQ.i).AND.(numiu2(x,i).GE.2)
     &.AND.(sau2(x,i).LT.1.0).AND.(sav2(x,i).LT.1.0) )then
       hu=4
       hv=4
      elseif((z.GT.d(x)).AND.(z.LE.d(x+1))
     &.AND.(binkan.EQ.i).AND.(numiu2(x,i).GE.2)
     &.AND.(sau2(x,i).GT.1.0).AND.(sav2(x,i).LT.1.0) )then
       hu=2
       hv=4
      elseif((z.GT.d(x)).AND.(z.LE.d(x+1))
     &.AND.(binkan.EQ.i).AND.(numiu2(x,i).GE.2)
     &.AND.(sau2(x,i).LT.1.0).AND.(sav2(x,i).GT.1.0) )then
       hu=4
       hv=2
      else
       hu=2
       hv=2
      end if

      if((z.GT.d(x)).AND.(z.LE.d(x+1))
     &.AND.(binkan.EQ.i).AND.(numiu2(x,i).GE.2)
     &.AND.(abs(u-sumui2(x,i)/numiu2(x,i)).LE.sau2(x,i)*hu)
     &.AND.(abs(v-sumvi2(x,i)/numiu2(x,i)).LE.sav2(x,i)*hv) )then
      write(40,250)z,u,v,p,binkan
      sumui3(x,i)=sumui3(x,i)+u
      sumvi3(x,i)=sumvi3(x,i)+v
      numiu3(x,i)=numiu3(x,i)+1
      numiv3(x,i)=numiv3(x,i)+1

      elseif((z.GT.d(x)).AND.(z.LE.d(x+1))
     &.AND.(binkan.EQ.i).AND.(numiu2(x,i).GE.2)
     &.AND.(abs(u-sumui2(x,i)/numiu2(x,i)).LE.sau2(x,i)*hu)
     &.AND.(abs(v-sumvi2(x,i)/numiu2(x,i)).GT.sav2(x,i)*hv) )then
      write(40,250)z,u,9999.0,p,binkan
      sumui3(x,i)=sumui3(x,i)+u
      numiu3(x,i)=numiu3(x,i)+1

      elseif((z.GT.d(x)).AND.(z.LE.d(x+1))
     &.AND.(binkan.EQ.i).AND.(numiu2(x,i).GE.2)
     &.AND.(abs(u-sumui2(x,i)/numiu2(x,i)).GT.sau2(x,i)*hu)
     &.AND.(abs(v-sumvi2(x,i)/numiu2(x,i)).LE.sav2(x,i)*hv) )then
      write(40,250)z,9999.0,v,p,binkan
      sumvi3(x,i)=sumvi3(x,i)+v
      numiv3(x,i)=numiv3(x,i)+1 
      end if

      end do
 500  continue
      close(10)


      goto 650




**** あるビンの平均に対する標準偏差をすべてのケースで求める
      do x=1,301
      do k=1,40
      numu(x,k)=0
      numv(x,k)=0
      sau3(x,k)=0
      sav3(x,k)=0
      end do
      end do

      do x=1,300
      do i=1,40
      do k=1,40
      if((numiu2(x,i).GE.10).AND.(numiu2(x,k).GE.10))then
      sau3(x,k)=((sumui2(x,i)/numiu2(x,i))-
     &(sumui2(x,k)/numiu2(x,k)))**2+sau3(x,k)
      numu(x,k)=numu(x,k)+1 
      end if

      if((numiv2(x,i).GE.10).AND.(numiv2(x,k).GE.10))then
      sav3(x,k)=((sumvi2(x,i)/numiv2(x,i))-
     &(sumvi2(x,k)/numiv2(x,k)))**2+sav3(x,k) 
       numv(x,k)=numv(x,k)+1 
      end if
      end do
      end do
      end do

      do x=1,300
      sau3min=9999
      sav3min=9999
      do k=1,40

       if(numu(x,k).GE.2)then
       sau3(x,k)=sqrt(sau3(x,k)/(numu(x,k)-1))
       end if

       if(numv(x,k).GE.2)then
       sav3(x,k)=sqrt(sav3(x,k)/(numv(x,k)-1))
       end if

       if((numu(x,k).GE.2).AND.(sau3(x,k).LT.sau3min))then
       sau3min=sau3(x,k)
       umink(x)=k
       write(*,*)sau3min,k
       end if

       if((numv(x,k).GE.2).AND.(sav3(x,k).LT.sav3min))then
       sav3min=sav3(x,k)
       vmink(x)=k
       end if

      end do
      end do

      do x=1,300
       if(numu(x,1).GE.2)then
c*       write(25,'(2(x,i4),10(x,f6.3))')x,umink(x),(sau3(x,k),k=1,10)
       end if
      end do
***
 600  format(f8.2,2(x,f5.2,x,i6),2(x,i5))
      do x=1,300
      if((numiu2(x,umink(x)).GE.10).AND.(numiv2(x,vmink(x)).GE.10))then
      write(30,600)d(x),
     &sumui2(x,umink(x))/numiu2(x,umink(x)),numiu2(x,umink(x)),
     &sumvi2(x,vmink(x))/numiv2(x,vmink(x)),numiv2(x,vmink(x)),
     &umink(x),vmink(x)
      end if
      end do


***!!!!
 650  do x=1,300
      heikinu=0
      kou=0
      heikinv=0
      kov=0
      do i=1,40
      if(numiu3(x,i).GE.5)then
      heikinu=heikinu+sumui3(x,i)/numiu3(x,i)
      kou=kou+1
      end if
      if(numiv3(x,i).GE.5)then
      heikinv=heikinv+sumvi3(x,i)/numiv3(x,i)
      kov=kov+1
      end if
      end do
      if((kou.GT.0).AND.(kov.GT.0))then
      write(35,600)d(x),heikinu/kou,kou,heikinv/kov,kov
      end if
      end do
***
 700  format(2(x,i3),(x,f5.2),21(x,f3.0))
 701  format(2(x,i3),(x,f5.2),20(x,f3.0))
      do x=1,300
      do i=1,20
      if(numiu2(x,i).GE.10)then
      write(25,700)x,i,sumui3(x,i)/numiu3(x,i),(numa(x,i,j),j=1,21)
      write(25,701)x,i,sumui3(x,i)/numiu3(x,i),(numa(x,i,j),j=22,41)
      end if
      end do
      end do

***
      end do
 1000 continue
      stop
      end

      program   ladcp7
* Shear ¤ò ++

      implicit  none
      integer   n,i,x,nn
      real      numavu,sumavu,numu(40),avu(40),pau(40),
     &          megianu(40),sau1(40),sau2(40),sau3(40),
     &          d(301),sau1min,sau1i,punummax,punummaxi
      character sta*3

      open(5,file='/home/ocg/kanae/newf77/staname',
     &   form='formatted',status='old')

 1    format(a3,3x,i1,3x,i2,x,f5.2,3x,i3,x,f5.2)
 2    format(2(x,f15.4),x,i10)
 3    format(19(x,f5.0))
 10   format(f7.0,2(x,f7.2,x,f5.0))

      do nn=1,29
      read(5,1,end=1000)sta
      write(*,1)sta

*****************************
      open(10,file='/home/ocg/kanae/c'//sta//'/shear5em^memo',
     &               form='formatted',status='old')
      open(20,file='/home/ocg/kanae/c'//sta//'/ladcp7em^memo',
     &               form='formatted',status='new')
*********************************************************
      do n=1,300
      read(10,*,end=100)x,d(x),(numu(i),i=1,10)
      read(10,*)x,d(x),(numu(i),i=11,20)
      read(10,*)x,d(x),(numu(i),i=21,30)
      read(10,*)x,d(x),(numu(i),i=31,40)
      read(10,*)x,d(x),(avu(i),i=1,10)
      read(10,*)x,d(x),(avu(i),i=11,20)
      read(10,*)x,d(x),(avu(i),i=21,30)
      read(10,*)x,d(x),(avu(i),i=31,40)
      read(10,*)x,d(x),(pau(i),i=1,10)
      read(10,*)x,d(x),(pau(i),i=11,20)
      read(10,*)x,d(x),(pau(i),i=21,30)
      read(10,*)x,d(x),(pau(i),i=31,40)
      read(10,*)x,d(x),(megianu(i),i=1,10)
      read(10,*)x,d(x),(megianu(i),i=11,20)
      read(10,*)x,d(x),(megianu(i),i=21,30)
      read(10,*)x,d(x),(megianu(i),i=31,40)
      read(10,*)x,d(x),(sau1(i),i=1,10)
      read(10,*)x,d(x),(sau1(i),i=11,20)
      read(10,*)x,d(x),(sau1(i),i=21,30)
      read(10,*)x,d(x),(sau1(i),i=31,40)
      read(10,*)x,d(x),(sau2(i),i=1,10)
      read(10,*)x,d(x),(sau2(i),i=11,20)
      read(10,*)x,d(x),(sau2(i),i=21,30)
      read(10,*)x,d(x),(sau2(i),i=31,40)
      read(10,*)x,d(x),(sau3(i),i=1,10)
      read(10,*)x,d(x),(sau3(i),i=11,20)
      read(10,*)x,d(x),(sau3(i),i=21,30)
      read(10,*)x,d(x),(sau3(i),i=31,40)

      sau1min=9999.9
      do i=1,40
      if((d(x).GE.1000.0).AND.
     &(abs(pau(i)).LT.0.2).AND.(abs(sau2(i)).LT.0.5)
     &.AND.(sau1min.GT.sau1(i)))then
      sau1min=sau1(i)
      sau1i=i
      end if
      end do

      punummax=0
      do i=1,40
      if((d(x).GE.1000.0).AND.
     &(abs(pau(i)).LT.0.2).AND.(abs(sau2(i)).LT.0.5)
     &.AND.(punummax.LT.numu(i)))then
      punummax=numu(i)
      punummaxi=i
      end if
      end do 


      do i=1,40
      if((d(x).GE.1000.0).AND.
     &(abs(pau(i)).LT.0.2).AND.(abs(sau2(i)).LT.0.5)
     &.AND.(sau1(i).LT.1.0)
     &)then
      sumavu=sumavu+avu(i)
      numavu=numavu+1
      sumavu=avu(sau1i)
      numavu=1      

      elseif((d(x).LT.1000.0).AND.(numu(i).GE.10))then
      sumavu=sumavu+avu(i)
      numavu=numavu+1
      end if
      end do

      if(numavu.GE.1)then
      write(20,10)d(x),sumavu/numavu,numavu,0,0
      else
      write(20,10)d(x),9999,9999,0,0
      end if
      sumavu=0
      numavu=0

      end do
 100  continue

      end do
 1000 continue

      stop
      end

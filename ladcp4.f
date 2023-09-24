      program   ladcp4
*+++ Shear ++++
      implicit  none
      integer   nn,x,n,k,i,j,last
      real      d(2000),dshu,dshv,avdshu(2000),avdshv(2000),
     &          sumshu(2000),sumshv(2000),
     &          numv(2000),numu(2000),m,a,l,
     &          prof,da(8000,40,9),binkan,kyori
      character sta*2

      open(5,file='/home/ocg/kanae/LEG2/stanamekh03',
     &   form='formatted',status='old')

      do nn=1,99
 1    format(a2)
 2    format(x,f7.2,2(x,f10.2),x,f7.0,3(x,f3.0))
      read(5,1)sta
      write(*,1)sta
******************************************************
      open(10,file='/home/ocg/kanae/LEG2/C'//sta//'T/vzaup',
     &               form='formatted',status='old')
      open(20,file='/home/ocg/kanae/LEG2/C'//sta//'T/shup',
     &               form='formatted',status='new')
******************************************************

      last=0
      a=0

      do x=1,2000
         d(x)=4.0*x
      end do

      do x=1,2000
          dshu=0.0
          dshv=0.0
          avdshu(x)=0
          sumshu(x)=0
          numu(x)=0
          avdshv(x)=0
          sumshv(x)=0
          numv(x)=0
      end do
c
      read(10,*)(da(1,1,j),j=1,9)
cccccccccccccccccccccccccccccccccccccccccccccccccc
      do n=1,8000
      prof=da(n,1,2)
c
        do k=2,40
          read(10,*,end=300)(da(n,k,j),j=1,9)
c
            if(da(n,k,2).EQ.prof)then
            else if(da(n,k,2).GT.prof)then
               do j=1,9
               da(n+1,1,j)=da(n,k,j)
               end do
             last=k-1
             go to 100
            end if
        end do
 100    continue
c
c************290 do roop************
      do 290 k=1,last-1
**
        binkan=1
        do 200 i=2,last
        if( ((da(n,i,3)-da(n,k,3)).GE.binkan) )then
           a=i
           go to 150
         else
           goto 200
         end if
c
 150       l=a
              kyori=(da(n,k,4)+da(n,l,4))/2.0
c
              do x=1,2000
                if((kyori.GT.d(x-1)).AND.(kyori.LE.d(x)))then
                    m=x
                end if
              end do
c
              if((da(n,k,5).NE.9999.0).AND.(da(n,l,5).NE.9999.0).AND.
     &          (da(n,k,6).NE.9999.0).AND.(da(n,l,6).NE.9999.0).AND.
     &          ((da(n,l,4)-da(n,k,4)).NE.0)
     &        )then
               dshu=(da(n,l,5)-da(n,k,5))/(da(n,l,4)-da(n,k,4))
               dshv=(da(n,l,6)-da(n,k,6))/(da(n,l,4)-da(n,k,4))
c
               write(20,2)kyori,dshu,dshv,da(n,k,2),
     &                  da(n,l,3)-da(n,k,3),da(n,l,3),da(n,k,3)
c
               sumshu(m)=sumshu(m)+dshu
               numu(m)=numu(m)+1
               sumshv(m)=sumshv(m)+dshv
               numv(m)=numv(m)+1
              end if
c
c
 200  continue
c
 290  continue
c    
      end do
 300  continue
c
      close(10)
      close(20)

      end do
      stop
      end


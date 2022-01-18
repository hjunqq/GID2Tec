    !>主程序
    program GID2Tec

    use datatype
    implicit none



    narg = iargc()
    if (narg >0) then
        call GETARG(1,arg)
        fpath = arg
    else
        inpunit = 10
        open(inpunit,file='inp')
        read(inpunit,*)text
        read(inpunit,*)fpath
    endif



    mshunit=1
    resunit=2
    open(mshunit,FILE=fpath(1:len_trim(fpath))//".flavia.msh")
    open(resunit,FILE=fpath(1:len_trim(fpath))//".flavia.res")

    isMeshGroup = .TRUE.
    nzone = 0
    do while(isMeshGroup)
        call readmsh
        call readres
        call writetecb
    enddo
    contains
    !>读取网格信息
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine readmsh
    character(150)   ::orgline,text
    integer         ::idxi,idxj,igroup,icoor,ielem,idim
    type(eleminfo),pointer::GroupElemHead

    isMeshGroup = .false.
    isOldFormat = .True.
    ngroup=0
    ncoor=0
    nelem=0
    ngroup=0
    !将坐标和单元的头尾节点指针置空
    NULLIFY(GroupHead)
    NULLIFY(GroupLast)
    nullify(CoorHead)
    nullify(CoorLast)
    nullify(ElemHead)
    nullify(ElemLast)
    do
        !读取一行数据
        read(mshunit,'(A150)',END=100)orgline
        orgline = adjustl(orgline)                            !for old format
        read(orgline(1:index(orgline,' ')),'(A20)')text
        !取数据的第一个单词
        select case(lowcase(trim(text)))
        case('#')
            print *,"Read A Comment"
            isOldFormat = .False.
        case('group')
            print *,"This Result is in Group"
            isMeshGroup = .true.
            isOldFormat = .False.
        case('end')
            print *,"End Group"
            goto 100
        case('mesh')
            !开始读取单元组节点信息
            ngroup=ngroup+1
            !为单元组分配内存
            ALLOCATE(PGroup)
            PGroup.index=ngroup
            PGroup.Ncoor=0
            PGroup.Nelem = 0

            if(.not.isOldFormat)then
                idxi=index(orgline,'MESH')+len("MESH")+2          !去掉双引号，所以要多加一个字符
                idxj=index(orgline,'dimension')-1-2                !去掉双引号，需要多减一个字符
                read(orgline(idxi:idxj),"(A70)")PGroup.GroupName

                idxi=index(orgline,'dimension')+len("dimension")
                idxj=index(orgline,'ElemType')-1
                read(orgline(idxi:idxj),"(I3)")PGroup.Dim

                idxi=index(orgline,'ElemType')+len("ElemType")
                idxj=index(orgline,'Nnode')-1
                read(orgline(idxi:idxj),"(A70)")PGroup.ElemType

                idxi=index(orgline,'Nnode')+len("Nnode")
                idxj=len_trim(orgline)
                read(orgline(idxi:idxj),"(I3)")PGroup.Nnode
            else

                orgline = lowcase(orgline)

                write(PGroup.GroupName,*)ngroup

                idxi=index(orgline,'dimension')+len("dimension")
                idxj=index(orgline,'elemtype')-1
                read(orgline(idxi:idxj),*)PGroup.Dim

                idxi=index(orgline,'elemtype')+len("elemtype")
                idxj=index(orgline,'nnode')-1
                read(orgline(idxi:idxj),*)PGroup.ElemType

                idxi=index(orgline,'nnode')+len("nnode")
                idxj=len_trim(orgline)
                read(orgline(idxi:idxj),*)PGroup.Nnode

            endif




            write(*,*)"Mesh          ",PGroup.GroupName(1:len_trim(PGroup.GroupName))
            write(*,*)"Dimension     ",PGroup.Dim
            write(*,*)"ElemType      ",PGroup.ElemType(1:len_trim(PGroup.ElemType))
            write(*,*)"Nnode         ",PGroup.Nnode
            write(*,*)
            !将读取到的单元组放到链表的结尾
            PGroup.next=>NULL()
            if(ASSOCIATED(GroupLast))then
                GroupLast.next=>PGroup
                GroupLast=>PGroup
            else
                GroupHead=>PGroup
                GroupLast=>PGroup
            endif
        case("coordinates")
            print *,"Begine To Read Coordinates"
            do
                read(mshunit,'(A150)',END=100)orgline
                if(isOldFormat)orgline = adjustl(orgline)
                read(orgline(1:index(orgline,' ')),'(A20)')text
                select case(lowcase(trim(text)))
                case("end")
                    exit
                    case default
                    ncoor=ncoor+1
                    allocate(PCoor)
                    allocate(Pcoor.val(PGroup.Dim))
                    read(orgline,*)Pcoor.index,PCoor.val
                    if(ncoor/=PCoor.Index)stop 'Coor Error!'
                    PCoor.next=>Null()
                    if(associated(CoorLast))then
                        CoorLast.next=>PCoor
                        CoorLast=>PCoor
                    else
                        CoorHead=>PCoor
                        CoorLast=>PCoor
                    endif
                end select
                PGroup.Ncoor=ncoor
            enddo
        case("elements")
            print *,"Begine To Read Elements"
            ielem=0
            do
                read(mshunit,'(A150)',END=100)orgline
                if(isOldFormat)orgline = adjustl(orgline)
                read(orgline(1:index(orgline,' ')),'(A20)')text
                select case(lowcase(trim(text)))
                case("end")
                    exit
                    case default
                    nelem=nelem+1
                    ielem=ielem+1
                    allocate(PElem)
                    allocate(PElem.node(PGroup.Nnode))
                    read(orgline,*)PElem.index,PElem.node,PElem.group
                    !if(nelem/=PElem.Index)stop 'Elem Error!'
                    PElem.next=>Null()
                    if(associated(ElemLast))then
                        ElemLast.next=>PElem
                        ElemLast=>PElem
                    else
                        ElemHead=>PElem
                        ElemLast=>PElem
                    endif
                    if(ielem==1)then
                        GroupELemHead=>PElem
                    endif
                end select
            enddo
            PGroup.Nelem=ielem
            !为单元组节点中的单元列表分配内存，然后将单元列表指针指向单元信息地址
            if(ielem>0)then
                allocate(PGroup.Elem(ielem))
                PElem=>GroupELemHead
                ielem=0
                do while(associated(PElem))
                    !if(PElem.group .eq. PGroup.index)then
                    !    ielem=ielem+1
                    !    PGroup.Elem(ielem)=PElem
                    !endif
                    ielem=ielem+1
                    PGroup.Elem(ielem)=PElem

                    PElem=>PElem.next
                enddo
            endif
            print *,"Thera're ",ielem,"elem in this group"
        end select
    enddo

    !将链表转换成动态数组格式，以方便以后读取
    !为单元组分配内存
100 allocate(Group(ngroup))
    !从链表的头开始依次将值赋予数组
    PGroup=>GroupHead
    igroup=0
    do while(associated(PGroup))
        igroup=igroup+1
        Group(igroup)=PGroup
        PGroup=>PGroup.next
    enddo
    allocate(Coor(ncoor))
    Pcoor=>CoorHead
    icoor=0
    do while(associated(PCoor))
        icoor=icoor+1
        Coor(icoor)=PCoor
        PCoor=>PCoor.Next
    enddo
    allocate(Elem(nelem))
    PElem=>ElemHead
    ielem=0
    do while(associated(PElem))
        ielem=ielem+1
        Elem(ielem)=PElem
        PElem=>PElem.Next
    enddo
    ndim = 0
    do igroup = 1,ngroup
        ndim = max(ndim,Group(igroup).Dim)
    enddo
    end subroutine
    !>读取结果信息
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine readres
    character(300)   ::orgline,text
    integer          ::idxi,idxj,ires,icomp,icoor,istep
    real            ::curtime
    real,allocatable::time(:)

    nres = 0
    nullify(ResHead)
    nullify(ResLast)
    do
        read(resunit,'(A300)',END=100)orgline
        read(orgline(1:index(orgline,' ')),'(A20)')text
        select case(lowcase(trim(text)))
        case("ongroup")
            print *, "Group Result"
        case("end")
            goto 100
        case("result")
            nres=nres+1
            Allocate(PRes)
            PRes.index=nres
            PRes.next=>Null()
            idxi=index(orgline,'"')+1
            idxj=len_trim(orgline)
            read(orgline(idxi:idxj),'(A150)')orgline
            idxi=index(orgline,'"')-1
            read(orgline(1:idxi),'(A70)')PRes.ResName

            idxi=index(orgline,'"')+3
            idxj=len_trim(orgline)
            read(orgline(idxi:idxj),'(A150)')orgline
            idxi=index(orgline,'"')-1
            read(orgline(1:idxi),'(A70)')PRes.AnaName

            idxi=index(orgline,'"')+2
            idxj=len_trim(orgline)
            read(orgline(idxi:idxj),'(A150)')orgline
            read(orgline,*)PRes.TimeAna

            idxi=index(orgline,' ')
            idxj=len_trim(orgline)
            read(orgline(idxi:idxj),'(A150)')orgline
            read(orgline,*)PRes.ResType

            if (Pres.ResType .eq. 'Scalar')then
                PRes.nval = 1
            elseif (PRes.ResType .eq. 'Vector')then
                PRes.nval = 4
            elseif(PRes.ResType .eq. 'Matrix')then
                PRes.nval = 6
            endif
            if(associated(ResLast))then
                ResLast.next=>PRes
                ResLast=>PRes
            else
                ResHead=>PRes
                ResLast=>PRes
            endif
            write(*,*)"ResultName           ",PRes.ResName(1:len_trim(PRes.ResName))
            write(*,*)"AnalysisName         ",PRes.AnaName(1:len_trim(PRes.AnaName))
            write(*,*)"TimeAnalysis         ",PRes.TimeAna
            write(*,*)"ResultType           ",PRes.ResType(1:len_trim(PRes.ResType))
            write(*,*)
        case("componentnames")
            allocate(PRes.CompName(PRes.nval))
            idxi=index(orgline,'"')+1
            idxj=len_trim(orgline)
            read(orgline(idxi:idxj),'(A150)')orgline
            idxi=index(orgline,'"')-1
            read(orgline(1:idxi),'(A70)')PRes.CompName(1)
            do icomp=2, PRes.nval
                idxi=index(orgline,'"')+1
                idxj=len_trim(orgline)
                read(orgline(idxi:idxj),'(A150)')orgline
                idxi=index(orgline,'"')+1
                idxj=len_trim(orgline)
                read(orgline(idxi:idxj),'(A150)')orgline
                idxi=index(orgline,'"')-1
                read(orgline(1:idxi),'(A70)')PRes.CompName(icomp)
            enddo
            if(len_trim(orgline)==0)then
                PRes.nval= PRes.nval-1
            endif
        case("values")
            nullify(PRes.ValHead)
            nullify(PRes.ValLast)
            icoor=0
            do
                read(resunit,'(A150)')orgline
                read(orgline(1:index(orgline,' ')),'(A20)')text
                select case(trim(text))
                case("End")
                    exit
                    case default
                    icoor=icoor+1
                    allocate(PRes.PVal)
                    allocate(PRes.PVal.dat(PRes.nval))
                    read(orgline,*)PRes.PVal.index,PRes.PVal.dat
                    !if(icoor/=PRes.PVal.index)stop 'Res Error!'
                    PRes.PVal.next=>Null()
                    if(associated(PRes.ValLast))then
                        PRes.ValLast.next=>PRes.PVal
                        PRes.ValLast=>PRes.PVal
                    else
                        PRes.ValHead=>PRes.PVal
                        PRes.ValLast=>PRes.PVal
                    endif
                end select
            enddo
            allocate(PRes.Val(icoor))
            PRes.PVal=>PRes.ValHead
            icoor=0
            do while(associated(PRes.PVal))
                icoor=icoor+1
                PRes.Val(icoor)=PRes.PVal
                PRes.PVal=>PRes.PVal.next
            enddo
            case default ! for Old Format
            if(isOldFormat)then
                nres=nres+1
                Allocate(PRes)
                PRes.index=nres
                PRes.next=>Null()
                Read(orgline,*)PRes.ResName,PRes.LoadType,PRes.TimeAna,PRes.DataType,PRes.DataLoc,PRes.DescComp

                if(ndim==2)then
                    if(Pres.DataType==1) then
                        PRes.nval = 1
                    elseif(Pres.DataType==2)then
                        PRes.nval = 2
                    elseif(PRes.DataType==3)then
                        PRes.nval = 4
                    endif
                elseif(ndim==3)then
                    if(Pres.DataType==1) then
                        PRes.nval = 1
                    elseif(Pres.DataType==2)then
                        PRes.nval = 3
                    elseif(PRes.DataType==3)then
                        PRes.nval = 6
                    endif
                endif

                if(PRes.DescComp>0)then
                    Allocate(Pres.CompName(Pres.nval))
                    do icomp = 1, Pres.nval
                        read(resunit,*)Pres.CompName(icomp)
                    enddo
                endif

                write(*,*)"ResultName           ",PRes.ResName(1:len_trim(PRes.ResName))
                write(*,*)"AnalysisName         ",PRes.AnaName(1:len_trim(PRes.AnaName))
                write(*,*)"TimeAnalysis         ",PRes.TimeAna
                write(*,*)"ResultType           ",PRes.DataType
                write(*,*)

                if(associated(ResLast))then
                    ResLast.next=>PRes
                    ResLast=>PRes
                else
                    ResHead=>PRes
                    ResLast=>PRes
                endif

                nullify(PRes.ValHead)
                nullify(PRes.ValLast)
                icoor=0
                do
                    read(resunit,'(A300)')orgline
                    read(orgline(1:index(orgline,' ')),'(A20)')text
                    icoor=icoor+1
                    allocate(PRes.PVal)
                    allocate(PRes.PVal.dat(PRes.nval))
                    read(orgline,*)PRes.PVal.index,PRes.PVal.dat
                    !if(icoor/=PRes.PVal.index)stop 'Res Error!'
                    PRes.PVal.next=>Null()
                    if(associated(PRes.ValLast))then
                        PRes.ValLast.next=>PRes.PVal
                        PRes.ValLast=>PRes.PVal
                    else
                        PRes.ValHead=>PRes.PVal
                        PRes.ValLast=>PRes.PVal
                    endif
                    if(icoor==ncoor)exit
                enddo
                allocate(PRes.Val(icoor))
                PRes.PVal=>PRes.ValHead
                icoor=0
                do while(associated(PRes.PVal))
                    icoor=icoor+1
                    PRes.Val(icoor)=PRes.PVal
                    PRes.PVal=>PRes.PVal.next
                enddo
            endif

        end select
    enddo
100 allocate(Res(nres))
    allocate(Time(nres))
    PRes=>ResHead
    ires=0
    do while(associated(PRes))
        ires=ires+1
        Res(ires)=PRes
        Time(ires)=Pres.TimeAna
        PRes=>Pres.Next
    enddo
    nstep = 1
    if(nres>0)then
        curtime = Time(1)
        do ires= 1,nres
            if(Time(ires)/=curtime) then
                curtime = (Time(ires))
                nstep = nstep + 1
            endif
        enddo
    endif
    end subroutine
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !>输出Tecplot二进制格式
    subroutine writetecb

    include 'tecio.f90'
    integer     ::igroup,icoor,ielem,idim,ires,inode,ival,istep,nvariables,jgroup
    integer     ::StepPtr,ZonePtr
    character(1)::CompName(3)=(/'X','Y','Z'/)
    character(70)::elemType
    integer i

    Title="Good Luck"     !<标题，不能为空
    ntres=0
    if(ndim==2)then
        Variables = "X,Y"
        nVariables = 2
    else
        Variables = "X,Y,Z"
        nVariables = 3
    endif


    do ires=1,nres/nstep
        do ival=1,Res(ires).nval
            if(allocated(Res(ires).CompName))then
                Variables=Variables(1:len_trim(Variables))// ','// &
                    Res(ires).CompName(ival)(1:len_trim(Res(ires).CompName(ival)))
                nvariables = nvariables + 1
            else
                if(Res(ires).nval>1)then
                    Variables=Variables(1:len_trim(Variables))// ','// CompName(ival) // '-' //&
                        Res(ires).ResName
                else
                    Variables=Variables(1:len_trim(Variables))// ','// &
                        Res(ires).ResName
                endif
                nvariables = nvariables + 1
            endif
        enddo
    enddo
    !Variables="x,y,p"            !<变量名，不能为空
    FName=fpath(1:len_trim(fpath))//".plt"                  !<文件名
    ScratchDir="."                 !<临时文件目录
    Debug = 1;                     !<是否输出调试信息，0是不输出，1是输出
    VIsDouble = 0;                 !<精度设置，0是单精度，1是双精度
    FileType  = 0;                 !<文件类型，0是完整，1是网格，2是结果
    FileFormat = 0;                !<文件格式，0是Tecplot binary,1是Tecplot subzone

    NULLCHR = CHAR(0)

    if(nzone==0)then
        ierr=TECINI142(Title(1:len_trim(Title))//NULLCHR, &
            Variables(1:len_trim(Variables))//NULLCHR, &
            FName(1:len_trim(FName))//NULLCHR, &
            ScratchDir//NULLCHR, &
            FileFormat, &
            FileType, &
            Debug, &
            VIsDouble)
        if(ierr .eq. -1)then
            print*,"Initiate Failed!"
            stop
        endif
    endif

    do istep = 1, nstep
        StepPtr = (istep-1)*nres/nstep + 1
        jgroup = 0
        do igroup=1,ngroup
            
            if(jgroup==igroup)cycle
            
            if(Group(igroup).nElem<=0)then
                jgroup = igroup + 1
            else
                jgroup = igroup
            endif
            
            ZoneTitle   = Group(jgroup).groupname   !!区名称
            ZoneType    = 5                         !<0=ORDERED，1=FELINESEG，2=FETRIANGLE
            !!3=FEQUADRILATERAL，4=FETETRAHEDRON
            !!5=FEBRICK，6=FEPOLYGON，7=FEPOLYHEDRON
            
            
            ElemType = lowcase(trim(adjustl(Group(jgroup).ElemType)))
            
            
            select case(ElemType)
            case('linear')
                ZoneType = 1
            case('triangle')
                ZoneType = 2
            case('quadrilateral')
                ZoneType = 3
            case('tetrahedron')
                ZoneType = 4
            case('hexahedra')
                ZoneType = 5
            end select
            NumPts      = Group(1).ncoor       !节点数
            NumElems    = Group(jgroup).nelem       !单元数
            NumFaces    = 8                         !多面体单元的面，其他的没有用
            ICellMax    = 0                         !* not used */
            JCellMax    = 0                         !* not used */
            KCellMax    = 0                         !* not used */
            SolTime     = Res(StepPtr).TimeAna                     !瞬态分析要用
            StrandID    = Group(jgroup).Index                         !* StaticZone */
            ParentZn    = 0                         !父区域，0表示没有，
            IsBlock     = 1                         !Block模式
            NFConns     = 0                         !接触面数
            FNMode      = 2                         !接触类型，0是局部1对1，1是局部1对多，2是全局1对1，3是全局1对多
            TotalNumFaceNodes = 1                   !面节点数，对于多边形有用
            NumConnectedBoundaryFaces = 1           !多边形有用
            TotalNumBoundaryConnections = 1         !多边形有用
            ShrConn     = 0                         !变量共享，0是不共享

            if(igroup.gt.1)then
                if(allocated(ShareVarFromZone))then
                    deallocate(ShareVarFromZone)
                    allocate(ShareVarFromZone(nvariables))
                else
                    allocate(ShareVarFromZone(nvariables))
                endif
                ShareVarFromZone = ZonePtr
            endif
            
            ierr = TECZNE142(ZoneTitle,&
                ZoneType,&
                NumPts,&
                NumElems,&
                NumFaces,&
                ICellMax,&
                JCellMax,&
                KCellMax,&
                SolTime,&
                StrandID,&
                ParentZn,&
                IsBlock,&
                NFConns,&
                FNMode,&
                TotalNumFaceNodes,&
                NumConnectedBoundaryFaces,&
                TotalNumBoundaryConnections, &
                PassiveVarList, &
                ValueLocation,&
                ShareVarFromZone,&
                ShrConn)
            if(ierr .eq. -1)then
                print*,"Create Zone Failed!"
                stop
            endif
            nzone = nzone + 1
            if(igroup.eq.1)then
                ZonePtr = nzone
            endif

            if(allocated(X))then
                deallocate(X)
                allocate(X(NumPts))
            else
                allocate(X(Numpts))
            endif
            if(allocated(Y))then
                deallocate(Y)
                allocate(Y(NumPts))
            else
                allocate(Y(Numpts))
            endif
            if(allocated(Z))then
                deallocate(Z)
                allocate(Z(NumPts))
            else
                allocate(Z(Numpts))
            endif
            if(allocated(P))then
                deallocate(P)
                allocate(P(NumPts))
            else
                allocate(P(Numpts))
            endif
            if(ndim==2)then
                do icoor=1,ncoor
                    x(icoor)=coor(icoor).val(1)
                    y(icoor)=coor(icoor).val(2)
                enddo
            elseif(ndim==3)then
                do icoor=1,ncoor
                    x(icoor)=coor(icoor).val(1)
                    y(icoor)=coor(icoor).val(2)
                    z(icoor)=coor(icoor).val(3)
                enddo
            endif
            IsDouble = 0
            if(igroup.eq.1)then
                if(ndim==2)then
                    ierr = TECDAT142(NumPts,X,IsDouble)
                    ierr = TECDAT142(NumPts,Y,IsDouble)
                elseif(ndim==3)then
                    ierr = TECDAT142(NumPts,X,IsDouble)
                    ierr = TECDAT142(NumPts,Y,IsDouble)
                    ierr = TECDAT142(NumPts,Z,IsDouble)
                endif
                do ires=1,nres/nstep
                    do ival = 1, res(ires+StepPtr -1 ).nval
                        do icoor = 1,ncoor
                            p(icoor) = res(ires + StepPtr -1).val(icoor).dat(ival)
                        enddo
                        ierr = TECDAT142(NumPts,P,IsDouble)
                    enddo
                enddo
            endif

            if(allocated(NData))then
                deallocate(NData)
                allocate(NData(group(jgroup).nnode*NumElems))
            else
                allocate(NData(group(jgroup).nnode*NumElems))
            endif
            do ielem=1,group(jgroup).nelem
                do inode=1,group(jgroup).nnode
                    ndata((ielem-1)*group(jgroup).nnode+inode)=group(jgroup).elem(ielem).node(inode)
                enddo
            enddo
            !NData = (/1,3,4,2,3,5,6,4/)
            ierr = TECNOD142(NData)
            if(ierr/=0)then
                print *,jgroup
            endif

        enddo
        if(allocated(ShareVarFromZone))deallocate(ShareVarFromZone)
    enddo

    if(.not.isMeshGroup)then
        ierr = TECEND142()
        if(ierr .eq. -1)then
            print*,"Closed File Failed!"
            stop
        endif
    endif
    end subroutine

    function lowcase(s) result(t)
    ! return string 's' in lowercase
    character(*), intent(in)	::s
    character(len(s))	::t
    integer	::i,diff
    t = s; diff = 65-97
    do i=1,len(t)
        if(ichar(t(i:i))>65.and.ichar(t(i:i))<=90)then
            ! if uppercase, make lowercase
            t(i:i) = char(ichar(t(i:i))-diff)
        endif
    enddo
    end function

    end program

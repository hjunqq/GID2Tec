    !>全局变量模块，声明此程序所需要的全局变量
    module datatype
    !> 坐标链表节点.
    !!@param index 坐标的索引值
    !!@param val 坐标的值
    !!@param next 指向下一个坐标节点
    type coorinfo
        integer         ::index
        real,allocatable::val(:)
        type(coorinfo),pointer::next
    end type
    !> 单元链表节点
    !!@param index 单元索引值
    !!@param group 单元所在组
    !!@param node 单元节点
    !!@param next 指向下一个单元节点
    type eleminfo
        integer         ::index, group
        integer,allocatable::node(:)
        type(eleminfo),pointer::next
    end type
    !> 单元组链表节点
    !!@param MeshName 单元组的名字
    !!@param ElemType 单元组类型
    !!@param index 单元组索引值
    !!@param Dim 单元组维数
    !!@param Nnode 每个单元节点数
    !!@param Ncoor 单元组中的节点数
    !!@param Nelem 单元组中的单元数
    !!@param Next 指向下一个节点
    !!@param Elem 单元组中的单元列表
    type Groupinfo
        character(70)   ::GroupName,ElemType
        integer         ::index,Dim,Nnode,Ncoor,Nelem
        type(Groupinfo),pointer::next
        type(eleminfo),dimension(:),pointer::Elem
        type(coorinfo),dimension(:),pointer::Coor
    end type
    !>结果值链表节点
    !!@param index 结果值索引
    !!@param dat 结果值
    !!@param next 下一个节点
    type ResValinfo
        integer         ::index
        real,allocatable    ::dat(:)
        type(ResValinfo),pointer::next
    end type
    !>结果组链表节点
    !!@param index 结果组索引
    !!@param ResName 结果组名称
    !!@param AnaName 分析类型
    !!@param ResType 结果类型；如向量，张量等
    !!@param CompName 结果列表每一列的名称
    !!@param Val 结果值
    !!@param ValHead 结果值链表头节点
    !!@param ValLast 结果值链表尾节点
    !!@param PVal 当前结果值节点
    !!@param next 下一个结果组
    type Resinfo
        character(70)   ::ResName,AnaName,ResType
        character(70),allocatable::CompName(:)
        character(70)   ::LoadDesc
        integer         ::index,nval,LoadType,DataType,DataLoc,DescComp,GaussPoint
        real            ::TimeAna
        type(ResValinfo),dimension(:),pointer::Val
        type(ResValinfo),pointer::ValHead,PVal,ValLast
        type(Resinfo),pointer::next
    end type
    
    type MeshGroupInfo
        character(70)   ::Name
        type(Groupinfo),dimension(:),pointer::Group
        type(Resinfo),dimension(:),pointer::Res
    endtype

    integer::mshunit,resunit,inpunit
    integer::ngroup,ncoor,nelem,nres,ntres,nstep,ndim,nzone
    logical::isMeshGroup,isOldFormat
    character(len=128)::arg
    integer::narg
    character(len=128)::fpath,text
    logical::eof
    type(Groupinfo),pointer::GroupHead,PGroup,GroupLast
    type(Groupinfo),dimension(:),pointer::Group
    type(coorinfo),pointer::CoorHead,PCoor,CoorLast
    type(coorinfo),dimension(:),pointer::Coor
    type(eleminfo),pointer::ElemHead,PElem,ElemLast
    type(eleminfo),dimension(:),pointer::Elem
    type(Resinfo),pointer::ResHead,PRes,ResLast
    type(Resinfo),dimension(:),pointer::Res

    integer             ::ierr
    !tecini142使用的变量
    CHARACTER(700)       ::Title,Variables,FName
    character(1)        ::ScratchDir
    character*1         ::NULLCHR
    INTEGER             ::FileType,Debug,VIsDouble,FileFormat
    !teczne142使用的变量
    CHARACTER(70)       ::ZoneTitle
    INTEGER             ::ZoneType,NumPts,NumElems,NumFaces
    INTEGER             ::ICellMax,JCellMax,KCellMax
    REAL(8)             ::SolTime
    INTEGER             ::StrandID,ParentZn,IsBlock,NFConns,FNMode
    INTEGER             ::TotalNumFaceNodes,NumConnectedBoundaryFaces
    INTEGER             ::TotalNumBoundaryConnections
    INTEGER,allocatable ::PassiveVarList(:),ValueLocation(:),ShareVarFromZone(:)
    INTEGER             ::ShrConn
    !tecdat142使用的变量
    INTEGER             ::N,IsDouble
    REAL,allocatable    ::FieldData(:),x(:),y(:),z(:),p(:)
    !tecnod142使用的变量
    INTEGER,allocatable ::NData(:)
    !tecface142使用的变量
    INTEGER,allocatable ::FaceConn(:)

    end module

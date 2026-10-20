program fortran_shape_lab
  use, intrinsic :: iso_fortran_env, only: real32, real64, int32, int64, output_unit, error_unit
  use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none

  interface
    function terminal_open(color, unicode, mouse, plain, alternate) bind(C, name='sl_terminal_open') result(res)
      import :: c_int
      integer(c_int), intent(inout) :: color, unicode, mouse
      integer(c_int), value :: plain, alternate
      integer(c_int) :: res
    end function terminal_open
    subroutine terminal_close() bind(C, name='sl_terminal_close')
    end subroutine terminal_close
    subroutine native_terminal_size(width, height, aspect) bind(C, name='sl_terminal_size')
      import :: c_int, c_double
      integer(c_int), intent(out) :: width, height
      real(c_double), intent(out) :: aspect
    end subroutine native_terminal_size
    subroutine terminal_sleep(milliseconds) bind(C, name='sl_terminal_sleep')
      import :: c_int
      integer(c_int), value :: milliseconds
    end subroutine terminal_sleep
    function terminal_events() bind(C, name='sl_terminal_events') result(res)
      import :: c_int
      integer(c_int) :: res
    end function terminal_events
    function terminal_read(buffer, capacity) bind(C, name='sl_terminal_read') result(res)
      import :: c_int, c_char
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_int), value :: capacity
      integer(c_int) :: res
    end function terminal_read
  end interface

  type :: mesh_type
    character(len=20) :: name = 'Cube'
    integer :: nv = 0, nt = 0, ne = 0
    logical :: smooth = .false.
    logical :: two_sided = .false.
    real(real32), allocatable :: v(:,:), n(:,:)
    integer, allocatable :: tri(:,:), edge(:,:)
  end type mesh_type

  type :: theme_type
    character(len=24) :: name = 'Neon Ice'
    integer :: sky_top(3), sky_horizon(3), floor_rgb(3), grid_rgb(3), star_rgb(3)
    integer :: base_a(3), base_b(3), edge_rgb(3)
    real(real32) :: key_pos(3), fill_pos(3), key_rgb(3), fill_rgb(3)
    real(real32) :: ambient, key_power, fill_power, rim_power
    real(real32) :: metallic, roughness, exposure, fog_strength
    logical :: stars = .true., grid = .true.
  end type theme_type

  integer, parameter :: resize_check_frames = 5
  integer, parameter :: max_shape = 20
  integer, parameter :: max_theme = 5
  integer, parameter :: cell_bytes = 64
  real(real32), parameter :: PI = 3.1415927_real32
  character(len=1), parameter :: ESC = achar(27)
  character(len=3), parameter :: UPPER_HALF = char(226)//char(150)//char(128)
  character(len=20), parameter :: shape_names(max_shape) = [character(len=20) :: &
    'Cube', 'Icosahedron', 'Sphere', 'Torus', 'Tetrahedron', 'Octahedron', 'Dodecahedron', &
    'Cylinder', 'Cone', 'Capsule', 'Triangular prism', 'Hexagonal prism', 'Pyramid', &
    'Rounded cube', 'Torus knot', 'Mobius strip', 'Star', 'Crystal', 'Spring', 'Ellipsoid']
  character(len=24), parameter :: theme_names(max_theme) = [character(len=24) :: &
    'Neon Ice', 'Sunset Chrome', 'Emerald Lab', 'Studio Neutral', 'Vaporwave']

  type(mesh_type) :: mesh
  type(theme_type) :: theme
  integer(int32), allocatable :: pixels(:,:), background(:,:)
  real(real32), allocatable :: zbuffer(:,:)
  real(real32), allocatable :: world(:,:), world_normals(:,:), screen_x(:), screen_y(:), vertex_depth(:)
  logical, allocatable :: projected(:)
  integer, allocatable :: previous_fg(:,:), previous_bg(:,:)
  character(len=1), allocatable :: previous_glyph(:,:)
  character(len=:), allocatable :: frame_text
  character(len=3), save :: decimal3(0:255)
  character(kind=c_char) :: input_bytes(1024)

  integer :: cols, rows, ph, new_cols, new_rows
  integer :: frame_number, used_bytes, shape_id, theme_id
  integer :: stop_requested = 0, frame_limit = 0, target_fps = 30, detail = 20
  integer :: requested_cols = 0, requested_rows = 0, view_top, view_height, hud_top, hud_bottom
  integer(c_int) :: color_mode = -1, unicode_mode = -1, mouse_mode = -1, event_flags, terminal_mode
  integer(c_int) :: plain_mode = 0, alternate_mode = 1
  real(real32) :: cell_aspect = 0.5_real32, requested_aspect = 0.0_real32, pixel_aspect, projection_scale
  real(real32) :: input_age = 0.0_real32
  logical :: interactive, draw_edges = .true., check_meshes = .false.
  logical :: initial_spin = .true., initial_hud = .true.
  integer(int64) :: clock_rate, frame_start, frame_end, previous_start
  real(real64) :: dt, elapsed, sleep_time, t, target_frame_time
  real(real32) :: orientation(3,3), angular_velocity(3), drag_vector(3)
  real(real32) :: camera_distance, auto_speed, interaction_quiet, inertia_speed
  logical :: auto_rotate, show_hud, force_redraw

  integer :: input_state, mouse_len
  character(len=64) :: mouse_text
  logical :: dragging, mouse_present, mouse_over_object
  integer :: mouse_x, mouse_y

  call init_decimal_table()
  shape_id = 1
  theme_id = 1
  call parse_options()
  if (check_meshes) then
    call validate_meshes()
    stop
  end if
  target_frame_time = 1.0_real64 / real(target_fps, real64)
  call load_theme(theme_id, theme)

  call rotation_matrix(-0.34_real32, 0.62_real32, 0.08_real32, orientation)
  camera_distance = 5.0_real32
  angular_velocity = 0.0_real32
  drag_vector = [0.0_real32,0.0_real32,-1.0_real32]
  auto_speed = 1.0_real32
  interaction_quiet = 0.0_real32
  auto_rotate = initial_spin
  show_hud = initial_hud
  force_redraw = .true.
  dragging = .false.
  mouse_present = .false.
  mouse_over_object = .false.
  input_state = 0
  mouse_len = 0
  mouse_text = ''
  mouse_x = 0
  mouse_y = 0
  t = 0.0_real64
  frame_number = 0

  terminal_mode = terminal_open(color_mode, unicode_mode, mouse_mode, plain_mode, alternate_mode)
  interactive = terminal_mode == 1
  if (.not.interactive) then
    color_mode = 0; unicode_mode = 0; mouse_mode = 0; frame_limit = 1
  end if
  call terminal_size(cols, rows)
  call ensure_buffers(cols, rows)
  call load_shape(shape_id, mesh)

  call system_clock(previous_start, clock_rate)

  do while (stop_requested == 0)
    call system_clock(frame_start)
    event_flags = terminal_events()
    if (iand(event_flags,1_c_int)/=0) exit
    if (iand(event_flags,2_c_int)/=0) then
      force_redraw=.true.; dragging=.false.; mouse_present=.false.; input_state=0
    end if

    if (frame_number > 0) then
      dt = real(frame_start - previous_start, real64) / real(clock_rate, real64)
      dt = min(max(dt, 0.001_real64), 0.080_real64)
    else
      dt = target_frame_time
    end if
    previous_start = frame_start

    if (mod(frame_number, resize_check_frames) == 0 .or. force_redraw) then
      call terminal_size(new_cols, new_rows)
      if (new_cols /= cols .or. new_rows /= rows) then
        cols = new_cols
        rows = new_rows
        call ensure_buffers(cols, rows)
        call load_shape(shape_id,mesh)
        force_redraw = .true.
        dragging=.false.; mouse_present=.false.
        if(interactive)write(output_unit,'(A)',advance='no') ESC//'[2J'
      end if
    end if

    if(interactive)call poll_input(real(dt,real32))
    if (stop_requested /= 0) exit
    call update_layout()

    ph = rows * 2
    if(auto_rotate .and. interactive)t = t + dt
    interaction_quiet = max(0.0_real32, interaction_quiet - real(dt,real32))

    if (.not. dragging) then
      inertia_speed=sqrt(dot_product(angular_velocity,angular_velocity))
      if(inertia_speed>1.0e-4_real32) &
        call apply_orientation_delta(angular_velocity/inertia_speed,inertia_speed*real(dt,real32))
      angular_velocity = angular_velocity * exp(-3.35_real32 * real(dt,real32))
    end if

    if (auto_rotate .and. interactive .and. .not. dragging .and. interaction_quiet <= 0.0_real32) then
      call apply_orientation_delta([0.0_real32,1.0_real32,0.0_real32], &
        0.58_real32*auto_speed*real(dt,real32))
      call apply_orientation_delta([0.0_real32,0.0_real32,1.0_real32], &
        0.075_real32*auto_speed*real(dt,real32))
    end if

    pixels = background
    zbuffer = huge(1.0_real32)

    call render_frame(cols, ph, real(t,real32), orientation)
    if(draw_edges)call draw_shape_outline(cols, ph)
    if (mouse_present) then
      call cursor_hits_shape(mouse_x, mouse_y, mouse_over_object)
      call draw_interaction_cursor(cols, ph, mouse_x, mouse_y, mouse_over_object, dragging)
    end if
    if(interactive)then
      call build_frame_delta(cols, rows, used_bytes, force_redraw)
      if (used_bytes > 0) write(output_unit,'(A)',advance='no') frame_text(1:used_bytes)
      if (show_hud) call write_hud()
      write(output_unit,'(A)',advance='no') ESC//'[0m'
      flush(output_unit)
    else
      call write_snapshot()
    end if
    force_redraw = .false.

    frame_number = frame_number + 1
    if(frame_limit>0)then
      if(frame_number>=frame_limit)exit
    end if
    call system_clock(frame_end)
    elapsed = real(frame_end - frame_start, real64) / real(clock_rate, real64)
    sleep_time = target_frame_time - elapsed
    if (sleep_time > 0.0005_real64) call terminal_sleep(int(sleep_time*1000.0_real64,c_int))
  end do

  flush(output_unit)
  call terminal_close()

contains

  subroutine fail(message)
    character(len=*),intent(in)::message
    call terminal_close()
    write(error_unit,'(A)')'shapes: '//message
    stop 2
  end subroutine fail

  function canonical(value) result(result_value)
    character(len=*),intent(in)::value
    character(len=len_trim(value))::result_value
    integer::i,k
    result_value=trim(value)
    do i=1,len(result_value)
      k=iachar(result_value(i:i))
      if(k>=65.and.k<=90)result_value(i:i)=achar(k+32)
      if(result_value(i:i)==' '.or.result_value(i:i)=='_')result_value(i:i)='-'
    end do
  end function canonical

  integer function positive_integer(value,label,limit) result(number)
    character(len=*),intent(in)::value,label
    integer,intent(in)::limit
    integer::ios
    if(len_trim(value)==0.or.verify(trim(value),'0123456789')/=0)call fail('invalid '//label)
    read(value,*,iostat=ios)number
    if(ios/=0)call fail('invalid '//label)
    if(number<1.or.number>limit)call fail(label//' out of range')
  end function positive_integer

  integer function lookup_name(value,names) result(id)
    character(len=*),intent(in)::value,names(:)
    integer::i,ios
    ios=1;id=0
    if(verify(value,'0123456789')==0)read(value,*,iostat=ios)id
    if(ios==0)then
      if(id>=1.and.id<=size(names))return
    end if
    do i=1,size(names)
      if(canonical(value)==canonical(trim(names(i))))then
        id=i;return
      end if
    end do
    call fail('unknown selection: '//trim(value))
  end function lookup_name

  subroutine option_value(index,value)
    integer,intent(inout)::index
    character(len=*),intent(inout)::value
    integer::stat
    if(len_trim(value)>0)return
    index=index+1
    if(index>command_argument_count())call fail('missing option value; use --help for usage')
    call get_command_argument(index,value,status=stat)
    if(stat/=0.or.len_trim(value)==0)call fail('invalid or overlong option value')
  end subroutine option_value

  subroutine parse_options()
    character(len=256)::option,value
    integer::i,j,k,ios,stat
    i=1
    do while(i<=command_argument_count())
      call get_command_argument(i,option,status=stat)
      if(stat/=0)call fail('overlong argument')
      value='';j=index(option,'=')
      if(j>0)then
        value=option(j+1:);option=option(:j-1)
        if(len_trim(value)==0)call fail('missing option value')
        select case(trim(option))
        case('--shape','--theme','--color','--size','--cell-aspect','--fps','--frames')
        case default
          call fail('this option does not take a value: '//trim(option))
        end select
      end if
      select case(trim(option))
      case('--help','-h')
        write(output_unit,'(A)') &
          'Shape Lab - portable, lit 3D shapes in your terminal', &
          'Usage: shapes [options]', &
          '  --shape NAME|N       Start with a shape (see --list-shapes)', &
          '  --theme NAME|N       Neon Ice, Sunset Chrome, Emerald Lab, Studio Neutral, Vaporwave', &
          '  --color MODE         auto, truecolor, 256, 16, mono (respects NO_COLOR)', &
          '  --ascii / --unicode  Override automatic character selection', &
          '  --snapshot           Print one ASCII frame without terminal controls', &
          '  --size COLSxROWS      Limit viewport; snapshot dimensions (1..1000 x 1..500)', &
          '  --cell-aspect RATIO   Character width / height (default: detect, or 0.5)', &
          '  --fps N              Frame limit, 1..120 (default 30)', &
          '  --frames N           Exit after N frames', &
          '  --no-mouse           Use keyboard controls only', &
          '  --ansi               Enable basic ANSI on an unrecognized terminal', &
          '  --no-alt-screen      Render on the main screen', &
          '  --no-spin / --no-hud / --no-edges', &
          '  --list-shapes        List all 20 shapes', &
          '  --check-meshes       Validate all generated meshes', &
          'Controls: drag / arrows / WASD rotate; wheel / Z X zoom;', &
          '1..9, 0 select shapes 1..10; N / Tab next; P previous; T theme;', &
          'Space pause; [ ] speed; R reset; E edges; H interface; Q quit.'
        stop
      case('--list-shapes')
        do k=1,max_shape
          write(output_unit,'(I2,2A)')k,'  ',trim(shape_names(k))
        end do
        stop
      case('--shape')
        call option_value(i,value);shape_id=lookup_name(trim(value),shape_names)
      case('--theme')
        call option_value(i,value);theme_id=lookup_name(trim(value),theme_names)
      case('--color')
        call option_value(i,value)
        select case(trim(value))
        case('auto');color_mode=-1
        case('truecolor','24bit');color_mode=16777216
        case('256');color_mode=256
        case('16');color_mode=16
        case('mono','none');color_mode=0
        case default;call fail('unknown color mode: '//trim(value))
        end select
      case('--size')
        call option_value(i,value);j=index(value,'x')
        if(j==0)j=index(value,'X')
        if(j<2.or.j>=len_trim(value))call fail('size must be COLSxROWS')
        requested_cols=positive_integer(value(:j-1),'column count',1000)
        requested_rows=positive_integer(trim(value(j+1:)),'row count',500)
      case('--cell-aspect')
        call option_value(i,value)
        if(verify(trim(value),'0123456789.eEdD+-')/=0)call fail('invalid cell aspect ratio')
        read(value,*,iostat=ios)requested_aspect
        if(ios/=0)call fail('invalid cell aspect ratio')
        if(.not.ieee_is_finite(requested_aspect))call fail('invalid cell aspect ratio')
        if(requested_aspect<0.2.or.requested_aspect>1.2)call fail('cell aspect must be between 0.2 and 1.2')
      case('--fps','--frames')
        call option_value(i,value);k=positive_integer(trim(value),'frame count or rate',100000000)
        if(option=='--fps')then
          if(k>120)call fail('fps must be within 1..120')
          target_fps=k
        else
          frame_limit=k
        end if
      case('--ascii');unicode_mode=0
      case('--unicode');unicode_mode=1
      case('--snapshot','--plain');plain_mode=1
      case('--ansi');plain_mode=-1
      case('--no-mouse');mouse_mode=0
      case('--no-alt-screen');alternate_mode=0
      case('--no-spin');initial_spin=.false.
      case('--no-hud');initial_hud=.false.
      case('--no-edges');draw_edges=.false.
      case('--check-meshes');check_meshes=.true.
      case default
        call fail('unknown option: '//trim(option)//'; use --help for usage')
      end select
      i=i+1
    end do
  end subroutine parse_options

  subroutine validate_meshes()
    integer::id,ti,level
    real(real32)::normal(3)
    do level=12,40,14
      detail=level
      do id=1,max_shape
        call load_shape(id,mesh)
        if(.not.all(ieee_is_finite(mesh%v)).or..not.all(ieee_is_finite(mesh%n)))call fail('nonfinite mesh')
        if(minval(mesh%tri)<1.or.maxval(mesh%tri)>mesh%nv)call fail('invalid triangle index')
        if(mesh%ne>0)then
          if(minval(mesh%edge)<1.or.maxval(mesh%edge)>mesh%nv)call fail('invalid edge index')
        end if
        do ti=1,mesh%nv
          if(abs(sum(mesh%n(:,ti)**2)-1.0_real32)>0.001_real32)call fail('invalid vertex normal')
          if(sqrt(sum(mesh%v(:,ti)**2))>1.301_real32)call fail('mesh exceeds framing radius')
        end do
        do ti=1,mesh%nt
          normal=cross3(mesh%v(:,mesh%tri(2,ti))-mesh%v(:,mesh%tri(1,ti)), &
                        mesh%v(:,mesh%tri(3,ti))-mesh%v(:,mesh%tri(1,ti)))
          if(sum(normal**2)<1.0e-14_real32)call fail('degenerate triangle in '//trim(mesh%name))
        end do
      end do
    end do
    write(output_unit,'(A)')'All 20 meshes passed validation at three detail levels.'
  end subroutine validate_meshes

  subroutine init_decimal_table()
    integer :: i
    do i = 0, 255
      write(decimal3(i),'(I3.3)') i
    end do
  end subroutine init_decimal_table

  subroutine load_shape(id, m)
    integer, intent(in) :: id
    type(mesh_type), intent(inout) :: m
    real(real32)::radius
    integer::i

    if (allocated(m%v)) deallocate(m%v)
    if (allocated(m%n)) deallocate(m%n)
    if (allocated(m%tri)) deallocate(m%tri)
    if (allocated(m%edge)) deallocate(m%edge)

    select case(id)
    case(1)
      call init_cube(m)
    case(2)
      call init_icosahedron(m)
    case(3)
      call init_sphere(m, detail, 2*detail)
    case(4)
      call init_torus(m, 3*detail, detail)
    case(5)
      call init_tetrahedron(m)
    case(6)
      call init_bipyramid(m,4)
    case(7)
      call init_dodecahedron(m)
    case(8)
      call init_prism(m,2*detail,.true.)
    case(9)
      call init_cone(m,2*detail)
    case(10)
      call init_capsule(m,detail,2*detail)
    case(11)
      call init_prism(m,3,.false.)
    case(12)
      call init_prism(m,6,.false.)
    case(13)
      call init_pyramid(m)
    case(14,17,20)
      call init_sphere(m,detail,2*detail)
      call deform_sphere(m,id)
    case(15,19)
      call init_tube(m,id==19,6*detail,max(10,detail/2))
    case(16)
      call init_mobius(m,4*detail,max(6,detail/3))
    case(18)
      call init_bipyramid(m,6)
    end select
    m%name=shape_names(id)
    radius=maxval(sqrt(sum(m%v**2,dim=1)))
    m%v=m%v*(1.3_real32/max(radius,0.001_real32))
    if(allocated(world))deallocate(world,world_normals,screen_x,screen_y,vertex_depth,projected)
    allocate(world(3,m%nv),world_normals(3,m%nv),screen_x(m%nv),screen_y(m%nv), &
      vertex_depth(m%nv),projected(m%nv))
    do i=1,m%nv
      m%n(:,i)=normalize3(m%n(:,i))
    end do
  end subroutine load_shape

  subroutine init_bipyramid(m,sides)
    type(mesh_type),intent(out)::m
    integer,intent(in)::sides
    integer::i,j
    real(real32)::angle,height
    m%nv=sides+2;m%nt=2*sides;m%ne=3*sides
    allocate(m%v(3,m%nv),m%n(3,m%nv),m%tri(3,m%nt),m%edge(2,m%ne))
    height=merge(1.0_real32,1.8_real32,sides==4)
    m%v(:,1)=[0.0_real32,height,0.0_real32]
    m%v(:,2)=[0.0_real32,-height,0.0_real32]
    do i=1,sides
      angle=2*PI*real(i-1,real32)/real(sides,real32)
      m%v(:,i+2)=[cos(angle),0.0_real32,sin(angle)]
      j=mod(i,sides)+3
      m%tri(:,2*i-1)=[1,i+2,j];m%tri(:,2*i)=[2,j,i+2]
      m%edge(:,3*i-2)=[1,i+2];m%edge(:,3*i-1)=[2,i+2];m%edge(:,3*i)=[i+2,j]
    end do
    m%n=m%v
  end subroutine init_bipyramid

  subroutine init_pyramid(m)
    type(mesh_type),intent(out)::m
    m%nv=5;m%nt=6;m%ne=8
    allocate(m%v(3,5),m%n(3,5),m%tri(3,6),m%edge(2,8))
    m%v=reshape([ &
      -1.0_real32,-.65_real32,-1.0_real32, 1.0_real32,-.65_real32,-1.0_real32, &
       1.0_real32,-.65_real32, 1.0_real32,-1.0_real32,-.65_real32, 1.0_real32, &
       0.0_real32,1.2_real32,0.0_real32],[3,5])
    m%n=m%v
    m%tri=reshape([1,2,3,1,3,4,1,5,2,2,5,3,3,5,4,4,5,1],[3,6])
    m%edge=reshape([1,2,2,3,3,4,4,1,1,5,2,5,3,5,4,5],[2,8])
  end subroutine init_pyramid

  subroutine init_dodecahedron(m)
    type(mesh_type),intent(out)::m
    type(mesh_type),allocatable::ico
    integer::i,j,k,a,b,n,ids(5),swap,ti,ne,edges(2,60)
    real(real32)::axis(3),tangent(3),bitangent(3),angles(5),temp
    logical::found
    allocate(ico)
    call init_icosahedron(ico)
    m%nv=20;m%nt=36
    allocate(m%v(3,20),m%n(3,20),m%tri(3,36))
    do i=1,20
      m%v(:,i)=normalize3(sum(ico%v(:,ico%tri(:,i)),dim=2));m%n(:,i)=m%v(:,i)
    end do
    ti=0;ne=0
    do i=1,12
      axis=normalize3(ico%v(:,i));tangent=normalize3(cross3(axis,[0.0_real32,1.0_real32,0.0_real32]))
      bitangent=cross3(axis,tangent);n=0
      do j=1,20
        if(.not.any(ico%tri(:,j)==i))cycle
        n=n+1;ids(n)=j
        angles(n)=atan2(dot_product(m%v(:,j),bitangent),dot_product(m%v(:,j),tangent))
      end do
      do j=2,5
        k=j
        do while(k>1)
          if(angles(k)>=angles(k-1))exit
          temp=angles(k);angles(k)=angles(k-1);angles(k-1)=temp
          swap=ids(k);ids(k)=ids(k-1);ids(k-1)=swap;k=k-1
        end do
      end do
      do j=2,4
        ti=ti+1;m%tri(:,ti)=[ids(1),ids(j),ids(j+1)]
      end do
      do j=1,5
        a=min(ids(j),ids(mod(j,5)+1));b=max(ids(j),ids(mod(j,5)+1));found=.false.
        do k=1,ne
          if(edges(1,k)==a.and.edges(2,k)==b)found=.true.
        end do
        if(.not.found)then
          ne=ne+1;edges(:,ne)=[a,b]
        end if
      end do
    end do
    m%ne=ne;allocate(m%edge(2,ne));m%edge=edges(:,:ne)
  end subroutine init_dodecahedron

  subroutine init_prism(m,sides,smooth)
    type(mesh_type),intent(out)::m
    integer,intent(in)::sides
    logical,intent(in)::smooth
    integer::i,j,ti
    real(real32)::angle,normal(3)
    m%nv=4*sides+2;m%nt=4*sides;m%smooth=smooth
    m%ne=0
    if(.not.smooth)m%ne=3*sides
    allocate(m%v(3,m%nv),m%n(3,m%nv),m%tri(3,m%nt),m%edge(2,m%ne))
    do i=1,sides
      angle=2*PI*real(i-1,real32)/real(sides,real32);normal=[cos(angle),0.0_real32,sin(angle)]
      m%v(:,i)=normal+[0.0_real32,-0.9_real32,0.0_real32]
      m%v(:,sides+i)=normal+[0.0_real32,0.9_real32,0.0_real32]
      m%v(:,2*sides+i)=m%v(:,i);m%v(:,3*sides+i)=m%v(:,sides+i)
      m%n(:,i)=normal;m%n(:,sides+i)=normal
      m%n(:,2*sides+i)=[0.0_real32,-1.0_real32,0.0_real32]
      m%n(:,3*sides+i)=[0.0_real32,1.0_real32,0.0_real32]
      j=mod(i,sides)+1;ti=4*i-3
      m%tri(:,ti)=[i,j,sides+j];m%tri(:,ti+1)=[i,sides+j,sides+i]
      m%tri(:,ti+2)=[4*sides+1,2*sides+j,2*sides+i]
      m%tri(:,ti+3)=[4*sides+2,3*sides+i,3*sides+j]
      if(.not.smooth)then
        m%edge(:,3*i-2)=[i,j];m%edge(:,3*i-1)=[sides+i,sides+j];m%edge(:,3*i)=[i,sides+i]
      end if
    end do
    m%v(:,4*sides+1)=[0.0_real32,-0.9_real32,0.0_real32]
    m%v(:,4*sides+2)=[0.0_real32,0.9_real32,0.0_real32]
    m%n(:,4*sides+1)=[0.0_real32,-1.0_real32,0.0_real32]
    m%n(:,4*sides+2)=[0.0_real32,1.0_real32,0.0_real32]
  end subroutine init_prism

  subroutine init_cone(m,sides)
    type(mesh_type),intent(out)::m
    integer,intent(in)::sides
    integer::i,j
    real(real32)::angle
    m%nv=3*sides+1;m%nt=2*sides;m%smooth=.true.
    allocate(m%v(3,m%nv),m%n(3,m%nv),m%tri(3,m%nt),m%edge(2,0))
    do i=1,sides
      angle=2*PI*real(i-1,real32)/real(sides,real32)
      m%v(:,i)=[cos(angle),-0.8_real32,sin(angle)]
      m%n(:,i)=normalize3([cos(angle),0.5_real32,sin(angle)])
      m%v(:,sides+i)=[0.0_real32,1.2_real32,0.0_real32]
      m%n(:,sides+i)=normalize3([cos(angle+PI/sides),0.5_real32,sin(angle+PI/sides)])
      m%v(:,2*sides+i)=m%v(:,i);m%n(:,2*sides+i)=[0.0_real32,-1.0_real32,0.0_real32]
      j=mod(i,sides)+1
      m%tri(:,2*i-1)=[i,j,sides+i];m%tri(:,2*i)=[3*sides+1,2*sides+i,2*sides+j]
    end do
    m%v(:,3*sides+1)=[0.0_real32,-0.8_real32,0.0_real32]
    m%n(:,3*sides+1)=[0.0_real32,-1.0_real32,0.0_real32]
  end subroutine init_cone

  subroutine init_capsule(m,nlat,nlon)
    type(mesh_type),intent(out)::m
    integer,intent(in)::nlat,nlon
    integer::j,k,idx,hem
    real(real32)::theta,phi,offset
    hem=max(4,nlat/2)
    call init_sphere(m,2*hem+1,nlon)
    m%v(:,1)=[0.0_real32,1.45_real32,0.0_real32]
    m%v(:,m%nv)=[0.0_real32,-1.45_real32,0.0_real32]
    idx=2
    do j=1,2*hem
      if(j<=hem)then
        theta=PI*0.5_real32*real(j,real32)/hem;offset=0.45_real32
      else
        theta=PI*0.5_real32*(1.0_real32+real(j-hem-1,real32)/hem);offset=-0.45_real32
      end if
      do k=0,nlon-1
        phi=2*PI*real(k,real32)/nlon
        m%n(:,idx)=[sin(theta)*cos(phi),cos(theta),sin(theta)*sin(phi)]
        m%v(:,idx)=m%n(:,idx)+[0.0_real32,offset,0.0_real32];idx=idx+1
      end do
    end do
  end subroutine init_capsule

  subroutine deform_sphere(m,kind)
    type(mesh_type),intent(inout)::m
    integer,intent(in)::kind
    integer::i,j,ids(3)
    real(real32)::n(3),u,v,r,normal(3)
    do i=1,m%nv
      n=m%n(:,i)
      select case(kind)
      case(14)
        m%v(:,i)=sign(abs(n)**0.45_real32,n)
        m%n(:,i)=normalize3(sign(abs(m%v(:,i))**(2.0_real32/0.45_real32-1.0_real32),m%v(:,i)))
      case(17)
        u=atan2(n(3),n(1));v=acos(clamp_real(n(2),-1.0_real32,1.0_real32))
        r=1.0_real32+0.24_real32*cos(5*u)*sin(v)**3+0.10_real32*cos(3*v)
        m%v(:,i)=n*r
      case(20)
        m%v(:,i)=n*[1.3_real32,0.72_real32,0.95_real32]
        m%n(:,i)=normalize3(n/[1.3_real32,0.72_real32,0.95_real32])
      end select
    end do
    if(kind/=17)return
    m%n=0.0_real32
    do i=1,m%nt
      ids=m%tri(:,i)
      normal=cross3(m%v(:,ids(2))-m%v(:,ids(1)),m%v(:,ids(3))-m%v(:,ids(1)))
      if(dot_product(normal,sum(m%v(:,ids),dim=2))<0)normal=-normal
      do j=1,3
        m%n(:,ids(j))=m%n(:,ids(j))+normal
      end do
    end do
  end subroutine deform_sphere

  function tube_center(u,spring) result(point)
    real(real32),intent(in)::u
    logical,intent(in)::spring
    real(real32)::point(3),r
    if(spring)then
      point=[cos(3*u),1.6_real32*(u/PI-1.0_real32),sin(3*u)]
    else
      r=0.85_real32+0.30_real32*cos(3*u)
      point=[r*cos(2*u),0.30_real32*sin(3*u),r*sin(2*u)]
    end if
  end function tube_center

  subroutine init_tube(m,spring,segments,sides)
    type(mesh_type),intent(out)::m
    logical,intent(in)::spring
    integer,intent(in)::segments,sides
    integer::rings,a,b,a1,b1,idx,ti,i00,i10,i11,i01
    real(real32)::u,v,center(3),tangent(3),normal(3),binormal(3),radius
    rings=segments
    if(spring)rings=rings+1
    m%nv=rings*sides;m%nt=2*segments*sides;m%smooth=.true.
    if(spring)then
      m%nv=m%nv+2*sides+2;m%nt=m%nt+2*sides
    end if
    allocate(m%v(3,m%nv),m%n(3,m%nv),m%tri(3,m%nt),m%edge(2,0))
    radius=merge(0.16_real32,0.145_real32,spring)
    do a=0,rings-1
      u=2*PI*real(a,real32)/segments;center=tube_center(u,spring)
      tangent=normalize3(tube_center(u+0.001_real32,spring)-tube_center(u-0.001_real32,spring))
      normal=normalize3(cross3(tangent,[0.0_real32,1.0_real32,0.0_real32]));binormal=cross3(tangent,normal)
      do b=0,sides-1
        v=2*PI*real(b,real32)/sides;idx=a*sides+b+1
        m%n(:,idx)=normal*cos(v)+binormal*sin(v);m%v(:,idx)=center+radius*m%n(:,idx)
      end do
    end do
    ti=0
    do a=0,segments-1
      a1=mod(a+1,rings)
      do b=0,sides-1
        b1=mod(b+1,sides);i00=a*sides+b+1;i10=a1*sides+b+1;i11=a1*sides+b1+1;i01=a*sides+b1+1
        ti=ti+1;m%tri(:,ti)=[i00,i10,i11];ti=ti+1;m%tri(:,ti)=[i00,i11,i01]
      end do
    end do
    if(.not.spring)return
    m%v(:,m%nv-1)=tube_center(0.0_real32,.true.);m%v(:,m%nv)=tube_center(2*PI,.true.)
    m%n(:,m%nv-1)=normalize3(tube_center(-0.001_real32,.true.)-tube_center(0.001_real32,.true.))
    m%n(:,m%nv)=normalize3(tube_center(2*PI+0.001_real32,.true.)-tube_center(2*PI-0.001_real32,.true.))
    do b=0,sides-1
      b1=mod(b+1,sides)
      m%v(:,rings*sides+b+1)=m%v(:,b+1)
      m%v(:,(rings+1)*sides+b+1)=m%v(:,(rings-1)*sides+b+1)
      m%n(:,rings*sides+b+1)=m%n(:,m%nv-1)
      m%n(:,(rings+1)*sides+b+1)=m%n(:,m%nv)
      ti=ti+1;m%tri(:,ti)=[m%nv-1,rings*sides+b+1,rings*sides+b1+1]
      ti=ti+1;m%tri(:,ti)=[m%nv,(rings+1)*sides+b1+1,(rings+1)*sides+b+1]
    end do
  end subroutine init_tube

  function mobius_point(u,v) result(point)
    real(real32),intent(in)::u,v
    real(real32)::point(3)
    point=[(1+v*cos(u/2))*cos(u),v*sin(u/2),(1+v*cos(u/2))*sin(u)]
  end function mobius_point

  subroutine init_mobius(m,segments,width)
    type(mesh_type),intent(out)::m
    integer,intent(in)::segments,width
    integer::a,b,idx,ti,i00,i01,i10,i11
    real(real32)::u,v,du(3),dv(3)
    m%nv=segments*(width+1);m%nt=2*segments*width;m%smooth=.true.;m%two_sided=.true.
    allocate(m%v(3,m%nv),m%n(3,m%nv),m%tri(3,m%nt),m%edge(2,0))
    do a=0,segments-1
      u=2*PI*real(a,real32)/segments
      do b=0,width
        v=0.8_real32*(real(b,real32)/width-0.5_real32);idx=a*(width+1)+b+1
        m%v(:,idx)=mobius_point(u,v)
        du=mobius_point(u+0.001_real32,v)-mobius_point(u-0.001_real32,v)
        dv=mobius_point(u,v+0.001_real32)-mobius_point(u,v-0.001_real32)
        m%n(:,idx)=normalize3(cross3(du,dv))
      end do
    end do
    ti=0
    do a=0,segments-1
      do b=0,width-1
        i00=a*(width+1)+b+1;i01=i00+1
        if(a==segments-1)then
          i10=width-b+1;i11=width-b
        else
          i10=(a+1)*(width+1)+b+1;i11=i10+1
        end if
        ti=ti+1;m%tri(:,ti)=[i00,i10,i11];ti=ti+1;m%tri(:,ti)=[i00,i11,i01]
      end do
    end do
  end subroutine init_mobius

  subroutine init_cube(m)
    type(mesh_type), intent(out) :: m
    integer :: i
    m%name = 'Cube'
    m%nv = 8; m%nt = 12; m%ne = 12; m%smooth = .false.
    allocate(m%v(3,m%nv), m%n(3,m%nv), m%tri(3,m%nt), m%edge(2,m%ne))
    m%v = reshape([ &
      -1.0_real32,-1.0_real32,-1.0_real32,  1.0_real32,-1.0_real32,-1.0_real32, &
       1.0_real32, 1.0_real32,-1.0_real32, -1.0_real32, 1.0_real32,-1.0_real32, &
      -1.0_real32,-1.0_real32, 1.0_real32,  1.0_real32,-1.0_real32, 1.0_real32, &
       1.0_real32, 1.0_real32, 1.0_real32, -1.0_real32, 1.0_real32, 1.0_real32 ], [3,8])
    do i=1,m%nv
      m%n(:,i) = normalize3(m%v(:,i))
    end do
    m%tri = reshape([ &
      1,3,2, 1,4,3, 5,6,7, 5,7,8, 1,5,8, 1,8,4, &
      2,3,7, 2,7,6, 1,2,6, 1,6,5, 4,8,7, 4,7,3 ], [3,12])
    m%edge = reshape([1,2,2,3,3,4,4,1, 5,6,6,7,7,8,8,5, 1,5,2,6,3,7,4,8],[2,12])
  end subroutine init_cube

  subroutine init_tetrahedron(m)
    type(mesh_type), intent(out) :: m
    integer :: i
    real(real32) :: s
    m%name='Tetrahedron'; m%nv=4; m%nt=4; m%ne=6; m%smooth=.false.
    allocate(m%v(3,4),m%n(3,4),m%tri(3,4),m%edge(2,6))
    s = 1.15_real32
    m%v = s*reshape([1.0_real32,1.0_real32,1.0_real32, -1.0_real32,-1.0_real32,1.0_real32, &
                     -1.0_real32,1.0_real32,-1.0_real32, 1.0_real32,-1.0_real32,-1.0_real32],[3,4])
    do i=1,4; m%n(:,i)=normalize3(m%v(:,i)); end do
    m%tri=reshape([1,2,3, 1,4,2, 1,3,4, 2,4,3],[3,4])
    m%edge=reshape([1,2,1,3,1,4,2,3,2,4,3,4],[2,6])
  end subroutine init_tetrahedron

  subroutine init_icosahedron(m)
    type(mesh_type), intent(out) :: m
    real(real32) :: phi
    integer :: i
    phi = (1.0_real32 + sqrt(5.0_real32))*0.5_real32
    m%name='Icosahedron'; m%nv=12; m%nt=20; m%smooth=.false.
    allocate(m%v(3,12),m%n(3,12),m%tri(3,20))
    m%v=reshape([ &
      -1.0_real32, phi,0.0_real32,  1.0_real32, phi,0.0_real32, -1.0_real32,-phi,0.0_real32, &
       1.0_real32,-phi,0.0_real32, 0.0_real32,-1.0_real32, phi, 0.0_real32,1.0_real32, phi, &
       0.0_real32,-1.0_real32,-phi, 0.0_real32,1.0_real32,-phi, phi,0.0_real32,-1.0_real32, &
       phi,0.0_real32,1.0_real32, -phi,0.0_real32,-1.0_real32, -phi,0.0_real32,1.0_real32],[3,12])
    do i=1,12
      m%v(:,i)=normalize3(m%v(:,i))*1.24_real32
      m%n(:,i)=normalize3(m%v(:,i))
    end do
    m%tri=reshape([ &
      1,12,6, 1,6,2, 1,2,8, 1,8,11, 1,11,12, &
      2,6,10, 6,12,5, 12,11,3, 11,8,7, 8,2,9, &
      4,10,5, 4,5,3, 4,3,7, 4,7,9, 4,9,10, &
      5,10,6, 3,5,12, 7,3,11, 9,7,8, 10,9,2 ],[3,20])
    call build_edges_from_triangles(m)
  end subroutine init_icosahedron

  subroutine init_sphere(m, nlat, nlon)
    type(mesh_type), intent(out) :: m
    integer, intent(in) :: nlat, nlon
    integer :: j,k,idx,ti,r0,r1,k1
    real(real32) :: th,ph,rr
    m%name='Sphere'; m%nv=2+(nlat-1)*nlon; m%nt=2*nlon*(nlat-1); m%ne=0; m%smooth=.true.
    allocate(m%v(3,m%nv),m%n(3,m%nv),m%tri(3,m%nt),m%edge(2,0))
    m%v(:,1)=[0.0_real32,1.22_real32,0.0_real32]
    m%n(:,1)=[0.0_real32,1.0_real32,0.0_real32]
    idx=2
    do j=1,nlat-1
      th=PI*real(j,real32)/real(nlat,real32)
      rr=sin(th)
      do k=0,nlon-1
        ph=2.0_real32*PI*real(k,real32)/real(nlon,real32)
        m%n(:,idx)=[rr*cos(ph),cos(th),rr*sin(ph)]
        m%v(:,idx)=1.22_real32*m%n(:,idx)
        idx=idx+1
      end do
    end do
    m%v(:,m%nv)=[0.0_real32,-1.22_real32,0.0_real32]
    m%n(:,m%nv)=[0.0_real32,-1.0_real32,0.0_real32]
    ti=1
    do k=0,nlon-1
      k1=mod(k+1,nlon)
      m%tri(:,ti)=[1,2+k,2+k1]; ti=ti+1
    end do
    do j=1,nlat-2
      r0=2+(j-1)*nlon
      r1=2+j*nlon
      do k=0,nlon-1
        k1=mod(k+1,nlon)
        m%tri(:,ti)=[r0+k,r1+k,r1+k1]; ti=ti+1
        m%tri(:,ti)=[r0+k,r1+k1,r0+k1]; ti=ti+1
      end do
    end do
    r0=2+(nlat-2)*nlon
    do k=0,nlon-1
      k1=mod(k+1,nlon)
      m%tri(:,ti)=[r0+k,m%nv,r0+k1]; ti=ti+1
    end do
  end subroutine init_sphere

  subroutine init_torus(m, nmajor, nminor)
    type(mesh_type), intent(out) :: m
    integer, intent(in) :: nmajor,nminor
    integer :: a,b,idx,ti,a1,b1,i00,i10,i11,i01
    real(real32) :: u,vv,cu,su,cv,sv,major_r,minor_r
    major_r=0.82_real32; minor_r=0.36_real32
    m%name='Torus'; m%nv=nmajor*nminor; m%nt=2*nmajor*nminor; m%ne=0; m%smooth=.true.
    allocate(m%v(3,m%nv),m%n(3,m%nv),m%tri(3,m%nt),m%edge(2,0))
    idx=1
    do a=0,nmajor-1
      u=2.0_real32*PI*real(a,real32)/real(nmajor,real32); cu=cos(u); su=sin(u)
      do b=0,nminor-1
        vv=2.0_real32*PI*real(b,real32)/real(nminor,real32); cv=cos(vv); sv=sin(vv)
        m%v(:,idx)=[(major_r+minor_r*cv)*cu, minor_r*sv, (major_r+minor_r*cv)*su]
        m%n(:,idx)=normalize3([cv*cu,sv,cv*su])
        idx=idx+1
      end do
    end do
    ti=1
    do a=0,nmajor-1
      a1=mod(a+1,nmajor)
      do b=0,nminor-1
        b1=mod(b+1,nminor)
        i00=1+a*nminor+b; i10=1+a1*nminor+b; i11=1+a1*nminor+b1; i01=1+a*nminor+b1
        m%tri(:,ti)=[i00,i10,i11]; ti=ti+1
        m%tri(:,ti)=[i00,i11,i01]; ti=ti+1
      end do
    end do
  end subroutine init_torus

  subroutine build_edges_from_triangles(m)
    type(mesh_type), intent(inout) :: m
    integer, allocatable :: tmp(:,:)
    integer :: i,j,a,b,count,k
    logical :: found
    allocate(tmp(2,3*m%nt)); count=0
    do i=1,m%nt
      do j=1,3
        a=m%tri(j,i); b=m%tri(1+mod(j,3),i)
        if (a>b) then; k=a; a=b; b=k; end if
        found=.false.
        do k=1,count
          if (tmp(1,k)==a .and. tmp(2,k)==b) then; found=.true.; exit; end if
        end do
        if (.not.found) then; count=count+1; tmp(:,count)=[a,b]; end if
      end do
    end do
    m%ne=count
    allocate(m%edge(2,count))
    if (count>0) m%edge=tmp(:,1:count)
    deallocate(tmp)
  end subroutine build_edges_from_triangles

  subroutine load_theme(id, th)
    integer,intent(in)::id
    type(theme_type),intent(out)::th
    select case(id)
    case(1)
      th%name='Neon Ice'
      th%sky_top=[3,5,14]; th%sky_horizon=[17,31,65]; th%floor_rgb=[5,9,20]
      th%grid_rgb=[21,70,115]; th%star_rgb=[150,205,255]
      th%base_a=[45,125,255]; th%base_b=[95,238,245]; th%edge_rgb=[216,246,255]
      th%key_pos=[-3.3_real32,3.6_real32,-3.8_real32]; th%fill_pos=[3.8_real32,-0.4_real32,-1.8_real32]
      th%key_rgb=[1.00_real32,0.93_real32,0.82_real32]; th%fill_rgb=[0.22_real32,0.58_real32,1.00_real32]
      th%ambient=.115_real32; th%key_power=8.2_real32; th%fill_power=2.6_real32; th%rim_power=.45_real32
      th%metallic=.38_real32; th%roughness=.24_real32; th%exposure=1.18_real32; th%fog_strength=.28_real32
      th%stars=.true.; th%grid=.true.
    case(2)
      th%name='Sunset Chrome'
      th%sky_top=[15,5,15]; th%sky_horizon=[78,24,46]; th%floor_rgb=[17,7,14]
      th%grid_rgb=[119,46,68]; th%star_rgb=[255,203,150]
      th%base_a=[255,93,51]; th%base_b=[255,190,75]; th%edge_rgb=[255,232,197]
      th%key_pos=[-2.8_real32,3.2_real32,-3.4_real32]; th%fill_pos=[3.6_real32,0.2_real32,-2.0_real32]
      th%key_rgb=[1.00_real32,0.63_real32,0.36_real32]; th%fill_rgb=[0.62_real32,0.22_real32,0.70_real32]
      th%ambient=.13_real32; th%key_power=8.8_real32; th%fill_power=2.1_real32; th%rim_power=.36_real32
      th%metallic=.62_real32; th%roughness=.18_real32; th%exposure=1.14_real32; th%fog_strength=.24_real32
      th%stars=.true.; th%grid=.true.
    case(3)
      th%name='Emerald Lab'
      th%sky_top=[2,10,8]; th%sky_horizon=[8,43,34]; th%floor_rgb=[3,14,11]
      th%grid_rgb=[15,79,59]; th%star_rgb=[155,255,216]
      th%base_a=[24,190,122]; th%base_b=[94,255,193]; th%edge_rgb=[210,255,235]
      th%key_pos=[-3.6_real32,3.5_real32,-3.0_real32]; th%fill_pos=[3.2_real32,-.3_real32,-2.6_real32]
      th%key_rgb=[0.74_real32,1.00_real32,0.88_real32]; th%fill_rgb=[0.10_real32,0.63_real32,0.46_real32]
      th%ambient=.12_real32; th%key_power=7.6_real32; th%fill_power=2.4_real32; th%rim_power=.32_real32
      th%metallic=.18_real32; th%roughness=.30_real32; th%exposure=1.15_real32; th%fog_strength=.25_real32
      th%stars=.false.; th%grid=.true.
    case(4)
      th%name='Studio Neutral'
      th%sky_top=[18,19,22]; th%sky_horizon=[47,49,53]; th%floor_rgb=[20,21,23]
      th%grid_rgb=[54,56,60]; th%star_rgb=[180,180,180]
      th%base_a=[176,181,190]; th%base_b=[112,121,137]; th%edge_rgb=[245,245,245]
      th%key_pos=[-3.8_real32,4.4_real32,-4.0_real32]; th%fill_pos=[4.0_real32,1.0_real32,-1.0_real32]
      th%key_rgb=[1.00_real32,0.96_real32,0.90_real32]; th%fill_rgb=[0.46_real32,0.55_real32,0.72_real32]
      th%ambient=.17_real32; th%key_power=9.5_real32; th%fill_power=2.0_real32; th%rim_power=.18_real32
      th%metallic=.72_real32; th%roughness=.21_real32; th%exposure=1.03_real32; th%fog_strength=.18_real32
      th%stars=.false.; th%grid=.false.
    case default
      th%name='Vaporwave'
      th%sky_top=[11,3,24]; th%sky_horizon=[57,13,77]; th%floor_rgb=[11,4,25]
      th%grid_rgb=[80,26,122]; th%star_rgb=[218,183,255]
      th%base_a=[255,61,183]; th%base_b=[45,219,255]; th%edge_rgb=[255,229,252]
      th%key_pos=[-3.0_real32,3.3_real32,-3.7_real32]; th%fill_pos=[3.3_real32,-.1_real32,-2.1_real32]
      th%key_rgb=[1.0_real32,.32_real32,.72_real32]; th%fill_rgb=[.20_real32,.71_real32,1.0_real32]
      th%ambient=.12_real32; th%key_power=8.1_real32; th%fill_power=2.7_real32; th%rim_power=.50_real32
      th%metallic=.46_real32; th%roughness=.20_real32; th%exposure=1.16_real32; th%fog_strength=.29_real32
      th%stars=.true.; th%grid=.true.
    end select
  end subroutine load_theme

  subroutine terminal_size(out_cols, out_rows)
    integer,intent(out)::out_cols,out_rows
    integer(c_int)::width,height
    real(c_double)::aspect
    call native_terminal_size(width,height,aspect)
    cell_aspect=real(aspect,real32)
    if(requested_aspect>0)cell_aspect=requested_aspect
    ! Leave the final column unused: even terminals without DECAWM cannot scroll on a bottom-right write.
    if(interactive)width=max(1_c_int,width-1_c_int)
    if(interactive)then
      if(requested_cols>0)width=min(width,int(requested_cols,c_int))
      if(requested_rows>0)height=min(height,int(requested_rows,c_int))
    else
      if(requested_cols>0)width=int(requested_cols,c_int)
      if(requested_rows>0)height=int(requested_rows,c_int)
    end if
    out_cols=max(1,min(int(width),1000));out_rows=max(1,min(int(height),500))
  end subroutine terminal_size

  subroutine update_layout()
    hud_top=0;hud_bottom=0
    if(show_hud)then
      if(rows>=5.and.cols>=20)hud_top=1
      if(rows>=3.and.cols>=10)hud_bottom=1
    end if
    view_top=2*hud_top+1;view_height=max(2,2*(rows-hud_top-hud_bottom))
    pixel_aspect=2.0_real32*cell_aspect
    projection_scale=min(real(cols,real32)*pixel_aspect,real(view_height,real32))*1.38_real32
    detail=max(16,min(40,nint(min(real(cols,real32)*pixel_aspect,real(view_height,real32))/2.5_real32)))
  end subroutine update_layout

  subroutine ensure_buffers(w,r)
    integer,intent(in)::w,r
    integer::height,needed,stat
    call update_layout()
    height=r*2; needed=r*(w*cell_bytes+40)+256
    if (allocated(pixels)) then
      if(size(pixels,1)==w .and. size(pixels,2)==height) return
      deallocate(pixels,background,zbuffer,previous_fg,previous_bg,previous_glyph)
      if(allocated(frame_text))deallocate(frame_text)
    end if
    allocate(pixels(w,height),background(w,height),zbuffer(w,height), &
      previous_fg(w,r),previous_bg(w,r),previous_glyph(w,r),stat=stat)
    if(stat/=0)call fail('unable to allocate the viewport; try a smaller --size')
    allocate(character(len=needed)::frame_text,stat=stat)
    if(stat/=0)call fail('unable to allocate the frame buffer; try a smaller --size')
    call make_background(w,height)
    previous_fg=-1;previous_bg=-1;previous_glyph=achar(0)
    zbuffer=huge(1.0_real32)
  end subroutine ensure_buffers

  subroutine rebuild_background()
    call make_background(cols,rows*2)
    previous_fg=-1;previous_bg=-1;previous_glyph=achar(0)
    force_redraw=.true.
  end subroutine rebuild_background

  subroutine render_frame(w,height,time,rot)
    integer,intent(in)::w,height
    real(real32),intent(in)::time,rot(3,3)
    real(real32)::bob,face_n(3),centroid(3),camera_pos(3),tri_normals(3,3)
    real(real32)::shadow_x,shadow_y,shadow_depth,object_scale
    logical::shadow_ok
    integer::i,j,ti,eidx,ids(3),base_col(3)
    real(real32)::edge_pulse

    bob=0.075_real32*sin(time*1.15_real32)
    object_scale=merge(1.03_real32,1.0_real32,mesh%smooth)
    do i=1,mesh%nv
      world(:,i)=matmul(rot,mesh%v(:,i))*object_scale
      world(2,i)=world(2,i)+0.10_real32+bob
      world_normals(:,i)=normalize3(matmul(rot,mesh%n(:,i)))
      call project_point(world(:,i),w,screen_x(i),screen_y(i),vertex_depth(i),projected(i))
    end do

    call project_point([0.0_real32,-1.30_real32,0.18_real32],w,shadow_x,shadow_y,shadow_depth,shadow_ok)
    if(shadow_ok)call draw_soft_shadow(w,height,shadow_x,shadow_y, &
      projection_scale*.26_real32/pixel_aspect,projection_scale*.045_real32,time)

    camera_pos=[0.0_real32,0.0_real32,-camera_distance]
    do ti=1,mesh%nt
      ids=mesh%tri(:,ti)
      if(.not.all(projected(ids)))cycle
      centroid=sum(world(:,ids),dim=2)/3.0_real32
      tri_normals=world_normals(:,ids)
      if(mesh%two_sided)then
        face_n=normalize3(cross3(world(:,ids(2))-world(:,ids(1)),world(:,ids(3))-world(:,ids(1))))
        do j=1,3
          if(dot_product(tri_normals(:,j),face_n)<0)tri_normals(:,j)=-tri_normals(:,j)
        end do
        if(dot_product(face_n,camera_pos-centroid)<0)then
          face_n=-face_n;tri_normals=-tri_normals
        end if
      else if(mesh%smooth)then
        face_n=normalize3(sum(tri_normals,dim=2))
      else
        face_n=normalize3(cross3(world(:,ids(2))-world(:,ids(1)),world(:,ids(3))-world(:,ids(1))))
        if(dot_product(face_n,sum(tri_normals,dim=2))<0)face_n=-face_n
      end if
      if(dot_product(face_n,camera_pos-centroid)<=0.015_real32)cycle
      call material_color(face_n,base_col)
      if(mesh%smooth)then
        call fill_lit_triangle(w,height, &
          screen_x(ids(1)),screen_y(ids(1)),vertex_depth(ids(1)),world(:,ids(1)),tri_normals(:,1), &
          screen_x(ids(2)),screen_y(ids(2)),vertex_depth(ids(2)),world(:,ids(2)),tri_normals(:,2), &
          screen_x(ids(3)),screen_y(ids(3)),vertex_depth(ids(3)),world(:,ids(3)),tri_normals(:,3),base_col)
      else
        call fill_lit_triangle(w,height, &
          screen_x(ids(1)),screen_y(ids(1)),vertex_depth(ids(1)),world(:,ids(1)),face_n, &
          screen_x(ids(2)),screen_y(ids(2)),vertex_depth(ids(2)),world(:,ids(2)),face_n, &
          screen_x(ids(3)),screen_y(ids(3)),vertex_depth(ids(3)),world(:,ids(3)),face_n,base_col)
      end if
    end do

    if(mesh%ne>0.and.draw_edges)then
      edge_pulse=.92_real32+.08_real32*sin(time*1.55_real32)
      do eidx=1,mesh%ne
        if(projected(mesh%edge(1,eidx)).and.projected(mesh%edge(2,eidx)))then
          call draw_glowing_line(w,height, &
            screen_x(mesh%edge(1,eidx)),screen_y(mesh%edge(1,eidx)),vertex_depth(mesh%edge(1,eidx)), &
            screen_x(mesh%edge(2,eidx)),screen_y(mesh%edge(2,eidx)),vertex_depth(mesh%edge(2,eidx)),edge_pulse)
        end if
      end do
    end if
  end subroutine render_frame

  subroutine material_color(normal,col)
    real(real32),intent(in)::normal(3)
    integer,intent(out)::col(3)
    real(real32)::mixv
    mixv=.17_real32+.22_real32*(normal(2)*.5_real32+.5_real32)+.08_real32*normal(1)*normal(3)
    mixv=clamp01(mixv)
    col=nint(real(theme%base_a,real32)*(1.0_real32-mixv)+real(theme%base_b,real32)*mixv)
  end subroutine material_color

  subroutine make_background(w,height)
    integer,intent(in)::w,height
    integer::x,y,hashv,irgb(3)
    real(real32)::fx,fy,q,dx,dy,vignette,glow,star,line_x,line_z,gridx,gridz
    real(real32)::c(3),a(3),b(3),floorc(3),gridc(3),starc(3),horizon
    horizon=.65_real32
    a=real(theme%sky_top,real32); b=real(theme%sky_horizon,real32)
    floorc=real(theme%floor_rgb,real32); gridc=real(theme%grid_rgb,real32); starc=real(theme%star_rgb,real32)
    do y=1,height
      fy=real(y-1,real32)/real(max(1,height-1),real32)
      do x=1,w
        fx=real(x-1,real32)/real(max(1,w-1),real32)
        dx=(fx-.50_real32)/.68_real32; dy=(fy-.42_real32)/.78_real32
        vignette=clamp01(1.08_real32-.35_real32*(dx*dx+dy*dy))
        glow=exp(-2.5_real32*(dx*dx+dy*dy))
        if(fy<horizon)then
          q=fy/horizon
          c=a*(1.0_real32-q)+b*q
          c=c+(6.0_real32+14.0_real32*glow)*[.35_real32,.50_real32,.80_real32]
          if(theme%stars)then
            hashv=modulo(x*1973+y*9277+x*y*17,7919)
            if(hashv<10)then
              star=20.0_real32+real(modulo(hashv*37,75),real32)
              c=c+starc*(star/255.0_real32)
            end if
          end if
        else
          q=(fy-horizon)/max(1.0_real32-horizon,.001_real32)
          c=floorc*(.78_real32+.35_real32*q)+real(theme%sky_horizon,real32)*(.07_real32*glow)
          if(theme%grid)then
            gridx=(fx-.5_real32)/max(.045_real32+q*.72_real32,.01_real32)
            gridz=1.0_real32/max(q+.055_real32,.055_real32)
            line_x=abs(gridx*10.0_real32-real(nint(gridx*10.0_real32),real32))
            line_z=abs(gridz*.85_real32-real(nint(gridz*.85_real32),real32))
            if(line_x<.040_real32 .or. line_z<.032_real32) c=c+gridc*(.20_real32*(1.0_real32-q))
          end if
        end if
        c=c*vignette
        irgb=[clamp255(c(1)),clamp255(c(2)),clamp255(c(3))]
        background(x,y)=pack_rgb(irgb(1),irgb(2),irgb(3))
      end do
    end do
  end subroutine make_background

  subroutine draw_soft_shadow(w,height,cx,cy,rx,ry,time)
    integer,intent(in)::w,height
    real(real32),intent(in)::cx,cy,rx,ry,time
    integer::x,y,minx,maxx,miny,maxy
    real(real32)::dx,dy,d2,alpha,glow_alpha,pulse
    minx=max(1,int(floor(cx-rx*1.55_real32))); maxx=min(w,int(ceiling(cx+rx*1.55_real32)))
    miny=max(1,int(floor(cy-ry*2.60_real32))); maxy=min(height,int(ceiling(cy+ry*2.60_real32)))
    pulse=.98_real32+.02_real32*sin(time*1.2_real32)
    do y=miny,maxy
      do x=minx,maxx
        dx=(real(x,real32)-cx)/max(rx*pulse,.001_real32)
        dy=(real(y,real32)-cy)/max(ry,.001_real32)
        d2=dx*dx+dy*dy
        if(d2>5.5_real32)cycle
        alpha=.54_real32*exp(-1.35_real32*d2)
        call blend_pixel(x,y,[0,0,2],alpha)
        glow_alpha=.030_real32*exp(-.72_real32*d2)
        call add_pixel(x,y,theme%edge_rgb,glow_alpha)
      end do
    end do
  end subroutine draw_soft_shadow

  subroutine fill_lit_triangle(w,height,x0,y0,z0,p0,n0,x1,y1,z1,p1,n1,x2,y2,z2,p2,n2,base_col)
    integer,intent(in)::w,height,base_col(3)
    real(real32),intent(in)::x0,y0,z0,p0(3),n0(3),x1,y1,z1,p1(3),n1(3),x2,y2,z2,p2(3),n2(3)
    integer::minx,maxx,miny,maxy,x,y,out_col(3)
    real(real32)::area,sgn,area_abs,px,py,e0,e1,e2,b0,b1,b2,q0,q1,q2,denom,depth
    real(real32)::pos(3),normal(3),view(3),ldir(3),camera_pos(3),radiance(3),linear(3),base(3)
    real(real32)::dist2,ndotv,fresnel,fog,noise,coat,coat_ndh,mixv
    real(real32)::mapped(3),fogc(3),hdir(3),base_a_linear(3),base_b_linear(3)

    minx=max(1,int(floor(min(x0,min(x1,x2))))); maxx=min(w,int(ceiling(max(x0,max(x1,x2)))))
    miny=max(1,int(floor(min(y0,min(y1,y2))))); maxy=min(height,int(ceiling(max(y0,max(y1,y2)))))
    if(minx>maxx .or. miny>maxy)return
    area=edge_function(x0,y0,x1,y1,x2,y2)
    if(abs(area)<1.0e-7_real32)return
    sgn=merge(-1.0_real32,1.0_real32,area<0.0_real32); area_abs=area*sgn
    base=(real(base_col,real32)/255.0_real32)**2.2_real32
    base_a_linear=(real(theme%base_a,real32)/255.0_real32)**2.2_real32
    base_b_linear=(real(theme%base_b,real32)/255.0_real32)**2.2_real32
    camera_pos=[0.0_real32,0.0_real32,-camera_distance]
    fogc=(real(theme%sky_horizon,real32)/255.0_real32)**2.2_real32

    do y=miny,maxy
      do x=minx,maxx
        px=real(x,real32)-.5_real32; py=real(y,real32)-.5_real32
        e0=edge_function(x1,y1,x2,y2,px,py)*sgn
        e1=edge_function(x2,y2,x0,y0,px,py)*sgn
        e2=edge_function(x0,y0,x1,y1,px,py)*sgn
        if(e0<-.001_real32 .or. e1<-.001_real32 .or. e2<-.001_real32)cycle
        b0=e0/area_abs; b1=e1/area_abs; b2=e2/area_abs
        q0=b0/z0; q1=b1/z1; q2=b2/z2; denom=q0+q1+q2
        if(denom<=1.0e-8_real32)cycle
        depth=1.0_real32/denom
        if(depth>=zbuffer(x,y))cycle
        pos=(q0*p0+q1*p1+q2*p2)*depth
        normal=normalize3((q0*n0+q1*n1+q2*n2)*depth)
        if(mesh%smooth)then
          mixv=clamp01(.24_real32+.30_real32*(normal(2)*.5_real32+.5_real32)+ &
                       .10_real32*(normal(1)*.5_real32+.5_real32))
          base=base_a_linear*(1.0_real32-mixv)+base_b_linear*mixv
        end if
        view=normalize3(camera_pos-pos); ndotv=max(.001_real32,dot_product(normal,view))

        linear=base*theme%ambient*(.62_real32+.38_real32*(normal(2)*.5_real32+.5_real32))
        ldir=normalize3(theme%key_pos-pos); dist2=dot_product(theme%key_pos-pos,theme%key_pos-pos)
        radiance=theme%key_rgb*(theme%key_power/(1.0_real32+.11_real32*dist2))
        linear=linear+brdf_light(normal,view,ldir,radiance,base,theme%metallic,theme%roughness)
        ldir=normalize3(theme%fill_pos-pos); dist2=dot_product(theme%fill_pos-pos,theme%fill_pos-pos)
        radiance=theme%fill_rgb*(theme%fill_power/(1.0_real32+.09_real32*dist2))
        linear=linear+brdf_light(normal,view,ldir,radiance,base,theme%metallic,min(.72_real32,theme%roughness+.10_real32))

        fresnel=(1.0_real32-ndotv)**3.0_real32
        linear=linear+theme%rim_power*fresnel*(real(theme%edge_rgb,real32)/255.0_real32)**2.0_real32
        hdir=normalize3(normalize3(theme%key_pos-pos)+view)
        coat_ndh=max(0.0_real32,dot_product(normal,hdir))
        coat=(coat_ndh**max(18.0_real32,150.0_real32*(1.0_real32-theme%roughness)))*.16_real32
        linear=linear+coat*theme%key_rgb

        fog=clamp01((depth-4.4_real32)/4.2_real32)*theme%fog_strength
        linear=linear*(1.0_real32-fog)+fogc*fog
        noise=(real(modulo(x*17+y*29,37),real32)/36.0_real32-.5_real32)*.0035_real32
        linear=max(linear+noise,0.0_real32)
        mapped=aces_tonemap(linear*theme%exposure)
        mapped=clamp_vec(mapped,0.0_real32,1.0_real32)**(1.0_real32/2.2_real32)
        out_col=[clamp255(mapped(1)*255.0_real32),clamp255(mapped(2)*255.0_real32),clamp255(mapped(3)*255.0_real32)]
        zbuffer(x,y)=depth; pixels(x,y)=pack_rgb(out_col(1),out_col(2),out_col(3))
      end do
    end do
  end subroutine fill_lit_triangle

  pure function brdf_light(n,v,l,radiance,base,metallic,roughness) result(outc)
    real(real32),intent(in)::n(3),v(3),l(3),radiance(3),base(3),metallic,roughness
    real(real32)::outc(3),h(3),f0(3),f(3),kd(3),spec(3),diff(3)
    real(real32)::ndotl,ndotv,ndoth,vdoth,a,a2,denom,d,k,g1,g2,g
    ndotl=max(0.0_real32,dot_product(n,l)); ndotv=max(.001_real32,dot_product(n,v))
    if(ndotl<=0.0_real32)then; outc=0.0_real32; return; end if
    h=normalize3(v+l); ndoth=max(.001_real32,dot_product(n,h)); vdoth=max(.001_real32,dot_product(v,h))
    a=max(.045_real32,roughness*roughness); a2=a*a
    denom=ndoth*ndoth*(a2-1.0_real32)+1.0_real32
    d=a2/(PI*denom*denom+.00001_real32)
    k=(roughness+1.0_real32)**2/8.0_real32
    g1=ndotv/(ndotv*(1.0_real32-k)+k); g2=ndotl/(ndotl*(1.0_real32-k)+k); g=g1*g2
    f0=.04_real32*(1.0_real32-metallic)+base*metallic
    f=f0+(1.0_real32-f0)*(1.0_real32-vdoth)**5
    spec=(d*g*f)/max(.001_real32,4.0_real32*ndotv*ndotl)
    kd=(1.0_real32-f)*(1.0_real32-metallic)
    diff=kd*base/PI
    outc=(diff+spec)*radiance*ndotl
  end function brdf_light

  pure function aces_tonemap(x) result(y)
    real(real32),intent(in)::x(3)
    real(real32)::y(3)
    y=(x*(2.51_real32*x+.03_real32))/(x*(2.43_real32*x+.59_real32)+.14_real32)
  end function aces_tonemap

  subroutine draw_glowing_line(w,height,x0,y0,z0,x1,y1,z1,pulse)
    integer,intent(in)::w,height
    real(real32),intent(in)::x0,y0,z0,x1,y1,z1,pulse
    integer::steps,i,x,y,ox,oy
    real(real32)::a,depth,r2,alpha
    steps=max(abs(nint(x1-x0)),abs(nint(y1-y0)))+1; steps=max(steps,1)
    do i=0,steps
      a=real(i,real32)/real(steps,real32);x=nint(x0+(x1-x0)*a);y=nint(y0+(y1-y0)*a)
      depth=1.0_real32/((1.0_real32-a)/z0+a/z1)
      do oy=-1,1
        do ox=-1,1
          r2=real(ox*ox+oy*oy,real32); alpha=pulse*.13_real32*exp(-.95_real32*r2)
          call blend_visible(x+ox,y+oy,depth,theme%edge_rgb,alpha,w,height)
        end do
      end do
      call plot_depth(x,y,depth-.025_real32,theme%edge_rgb,w,height)
    end do
  end subroutine draw_glowing_line

  subroutine draw_shape_outline(w,height)
    integer,intent(in)::w,height
    integer::x,y
    logical::boundary,near_surface
    real(real32)::empty_depth

    empty_depth=huge(1.0_real32)*.5_real32

    ! A restrained halo outside the silhouette softens the one-cell terminal edge.
    do y=2,height-1
      do x=2,w-1
        if(zbuffer(x,y)<empty_depth)cycle
        near_surface=zbuffer(x-1,y)<empty_depth .or. zbuffer(x+1,y)<empty_depth .or. &
                     zbuffer(x,y-1)<empty_depth .or. zbuffer(x,y+1)<empty_depth
        if(near_surface)call add_pixel(x,y,theme%edge_rgb,.075_real32)
      end do
    end do

    ! Keep every mesh readable against both the bright horizon and dark floor.
    do y=1,height
      do x=1,w
        if(zbuffer(x,y)>=empty_depth)cycle
        boundary=x==1 .or. x==w .or. y==1 .or. y==height
        if(.not.boundary)then
          boundary=zbuffer(x-1,y)>=empty_depth .or. zbuffer(x+1,y)>=empty_depth .or. &
                   zbuffer(x,y-1)>=empty_depth .or. zbuffer(x,y+1)>=empty_depth
        end if
        if(boundary)call blend_pixel(x,y,theme%edge_rgb,.42_real32)
      end do
    end do
  end subroutine draw_shape_outline

  subroutine cursor_hits_shape(cell_x,cell_y,hit)
    integer,intent(in)::cell_x,cell_y
    logical,intent(out)::hit
    integer::py
    real(real32)::empty_depth

    hit=.false.
    if(cell_x<1 .or. cell_x>cols .or. cell_y<1 .or. cell_y>rows)return
    py=max(1,min(rows*2,(cell_y-1)*2+1))
    empty_depth=huge(1.0_real32)*.5_real32
    hit=zbuffer(cell_x,py)<empty_depth .or. zbuffer(cell_x,min(py+1,rows*2))<empty_depth
  end subroutine cursor_hits_shape

  subroutine draw_interaction_cursor(w,height,cell_x,cell_y,over_shape,active)
    integer,intent(in)::w,height,cell_x,cell_y
    logical,intent(in)::over_shape,active
    integer::x,y,cx,cy,dx,dy,color(3),shadow(3)
    real(real32)::distance,radius

    cx=cell_x
    cy=(cell_y-1)*2+1
    if(cx<1 .or. cx>w .or. cy<1 .or. cy>height)return

    shadow=[1,3,8]
    if(active)then
      color=[255,255,255]
      radius=4.1_real32
    else if(over_shape)then
      color=theme%edge_rgb
      radius=3.25_real32
    else
      color=nint(.62_real32*real(theme%edge_rgb,real32)+.38_real32*real(theme%grid_rgb,real32))
      radius=3.25_real32
    end if

    do y=max(1,cy-6),min(height,cy+6)
      do x=max(1,cx-6),min(w,cx+6)
        dx=x-cx;dy=y-cy
        distance=sqrt(real(dx*dx+dy*dy,real32))
        if(abs(distance-radius)<1.05_real32)call blend_pixel(x,y,shadow,.76_real32)
        if(abs(distance-radius)<.48_real32)call blend_pixel(x,y,color,.92_real32)
      end do
    end do

    ! The open center keeps the target visible; short ticks make drag direction obvious.
    do dx=-1,1
      x=cx+dx
      if(x>=1 .and. x<=w)then
        call blend_pixel(x,cy,shadow,.72_real32)
        call blend_pixel(x,cy,color,.88_real32)
      end if
    end do
    do dy=-1,1
      y=cy+dy
      if(y>=1 .and. y<=height)then
        call blend_pixel(cx,y,shadow,.72_real32)
        call blend_pixel(cx,y,color,.88_real32)
      end if
    end do
  end subroutine draw_interaction_cursor

  subroutine project_point(point,w,sx,sy,depth,ok)
    real(real32),intent(in)::point(3)
    integer,intent(in)::w
    real(real32),intent(out)::sx,sy,depth
    logical,intent(out)::ok
    depth=point(3)+camera_distance
    if(depth<=.20_real32)then; sx=0.0; sy=0.0; ok=.false.; return; end if
    sx=(real(w,real32)+1)*.5_real32+point(1)*projection_scale/(depth*pixel_aspect)
    sy=real(view_top,real32)-.5_real32+real(view_height,real32)*.5_real32-point(2)*projection_scale/depth
    ok=.true.
  end subroutine project_point

  subroutine apply_orientation_delta(axis,angle)
    real(real32),intent(in)::axis(3),angle
    real(real32)::delta(3,3),axis_length

    axis_length=sqrt(dot_product(axis,axis))
    if(axis_length<1.0e-6_real32 .or. abs(angle)<1.0e-6_real32)return
    call axis_angle_matrix(axis/axis_length,angle,delta)
    orientation=matmul(delta,orientation)
    call orthonormalize_orientation()
  end subroutine apply_orientation_delta

  subroutine axis_angle_matrix(axis,angle,matrix)
    real(real32),intent(in)::axis(3),angle
    real(real32),intent(out)::matrix(3,3)
    real(real32)::x,y,z,c,s,t

    x=axis(1);y=axis(2);z=axis(3)
    c=cos(angle);s=sin(angle);t=1.0_real32-c
    matrix(1,1)=t*x*x+c
    matrix(1,2)=t*x*y-s*z
    matrix(1,3)=t*x*z+s*y
    matrix(2,1)=t*x*y+s*z
    matrix(2,2)=t*y*y+c
    matrix(2,3)=t*y*z-s*x
    matrix(3,1)=t*x*z-s*y
    matrix(3,2)=t*y*z+s*x
    matrix(3,3)=t*z*z+c
  end subroutine axis_angle_matrix

  subroutine orthonormalize_orientation()
    real(real32)::x_axis(3),y_axis(3),z_axis(3)

    x_axis=normalize3(orientation(:,1))
    y_axis=orientation(:,2)-dot_product(orientation(:,2),x_axis)*x_axis
    y_axis=normalize3(y_axis)
    z_axis=normalize3(cross3(x_axis,y_axis))
    y_axis=normalize3(cross3(z_axis,x_axis))
    orientation(:,1)=x_axis
    orientation(:,2)=y_axis
    orientation(:,3)=z_axis
  end subroutine orthonormalize_orientation

  subroutine trackball_vector(cell_x,cell_y,vector)
    integer,intent(in)::cell_x,cell_y
    real(real32),intent(out)::vector(3)
    real(real32)::screen_x,screen_y,radius,distance2

    radius=max(2.0_real32,min(real(cols,real32)*pixel_aspect,real(view_height,real32))*.42_real32)
    screen_x=(real(cell_x,real32)-(real(cols,real32)+1)*.5_real32)*pixel_aspect/radius
    screen_y=(real(view_top,real32)+real(view_height,real32)*.5_real32-real((cell_y-1)*2+1,real32))/radius
    distance2=screen_x*screen_x+screen_y*screen_y
    if(distance2<=1.0_real32)then
      vector=[screen_x,screen_y,-sqrt(max(0.0_real32,1.0_real32-distance2))]
    else
      vector=[screen_x/sqrt(distance2),screen_y/sqrt(distance2),0.0_real32]
    end if
    vector=normalize3(vector)
  end subroutine trackball_vector

  subroutine rotation_matrix(ax,ay,az,matrix)
    real(real32),intent(in)::ax,ay,az
    real(real32),intent(out)::matrix(3,3)
    real(real32)::cx,sx,cy,sy,cz,sz
    cx=cos(ax);sx=sin(ax);cy=cos(ay);sy=sin(ay);cz=cos(az);sz=sin(az)
    matrix(1,1)=cz*cy; matrix(1,2)=cz*sy*sx-sz*cx; matrix(1,3)=cz*sy*cx+sz*sx
    matrix(2,1)=sz*cy; matrix(2,2)=sz*sy*sx+cz*cx; matrix(2,3)=sz*sy*cx-cz*sx
    matrix(3,1)=-sy; matrix(3,2)=cy*sx; matrix(3,3)=cy*cx
  end subroutine rotation_matrix

  subroutine plot_depth(x,y,depth,color,w,height)
    integer,intent(in)::x,y,color(3),w,height
    real(real32),intent(in)::depth
    if(x<1.or.x>w.or.y<1.or.y>height)return
    if(depth<zbuffer(x,y))then; zbuffer(x,y)=depth; pixels(x,y)=pack_rgb(color(1),color(2),color(3)); end if
  end subroutine plot_depth

  subroutine blend_visible(x,y,depth,color,alpha,w,height)
    integer,intent(in)::x,y,color(3),w,height
    real(real32),intent(in)::depth,alpha
    if(x<1.or.x>w.or.y<1.or.y>height)return
    if(depth<=zbuffer(x,y)+.08_real32)call blend_pixel(x,y,color,alpha)
  end subroutine blend_visible

  subroutine blend_pixel(x,y,color,alpha)
    integer,intent(in)::x,y,color(3)
    real(real32),intent(in)::alpha
    integer::old(3),mixed(3); real(real32)::a
    a=clamp01(alpha); call unpack_rgb(pixels(x,y),old)
    mixed=nint(real(old,real32)*(1.0_real32-a)+real(color,real32)*a)
    pixels(x,y)=pack_rgb(mixed(1),mixed(2),mixed(3))
  end subroutine blend_pixel

  subroutine add_pixel(x,y,color,amount)
    integer,intent(in)::x,y,color(3)
    real(real32),intent(in)::amount
    integer::old(3),mixed(3)
    call unpack_rgb(pixels(x,y),old); mixed=old+nint(real(color,real32)*max(amount,0.0_real32))
    pixels(x,y)=pack_rgb(clamp255(real(mixed(1),real32)),clamp255(real(mixed(2),real32)),clamp255(real(mixed(3),real32)))
  end subroutine add_pixel

  subroutine build_frame_delta(w,terminal_rows,used,full)
    integer,intent(in)::w,terminal_rows
    integer,intent(out)::used
    logical,intent(in)::full
    integer::row,x,pos,last_fg,last_bg,fg,bg
    character(len=1)::glyph
    logical::changed,in_run
    pos=1; last_fg=-2; last_bg=-2; in_run=.false.
    do row=1,terminal_rows
      if(hud_top==1.and.row==1)cycle
      if(hud_bottom==1.and.row==terminal_rows)cycle
      in_run=.false.
      do x=1,w
        call cell_style(x,row,fg,bg,glyph)
        changed=full.or.fg/=previous_fg(x,row).or.bg/=previous_bg(x,row).or.glyph/=previous_glyph(x,row)
        if(.not.changed)then
          in_run=.false.; cycle
        end if
        if(.not.in_run)then
          call append_cursor(pos,row,x)
          last_fg=-2; last_bg=-2; in_run=.true.
        end if
        if(color_mode==0)then
          call append_glyph(pos,glyph)
        else if(fg==last_fg .and. bg==last_bg)then
          call append_glyph(pos,glyph)
        else if(fg==last_fg)then
          call append_bg(pos,bg); call append_glyph(pos,glyph); last_bg=bg
        else if(bg==last_bg)then
          call append_fg(pos,fg); call append_glyph(pos,glyph); last_fg=fg
        else
          call append_pair(pos,fg,bg); call append_glyph(pos,glyph); last_fg=fg; last_bg=bg
        end if
        previous_fg(x,row)=fg;previous_bg(x,row)=bg;previous_glyph(x,row)=glyph
      end do
    end do
    used=pos-1
  end subroutine build_frame_delta

  integer function terminal_color(packed) result(color)
    integer,intent(in)::packed
    integer,parameter::levels(0:5)=[0,95,135,175,215,255]
    integer,parameter::palette(3,0:15)=reshape([ &
      0,0,0, 170,0,0, 0,170,0, 170,85,0, 0,0,170, 170,0,170, 0,170,170, 170,170,170, &
      85,85,85, 255,85,85, 85,255,85, 255,255,85, 85,85,255, 255,85,255, 85,255,255, 255,255,255],[3,16])
    integer::rgb(3),cube(3),i,j,distance,best,gray,gray_distance
    if(color_mode==16777216)then
      color=packed;return
    end if
    color=0
    if(color_mode==0)return
    call unpack_rgb(packed,rgb)
    if(color_mode==256)then
      do j=1,3
        best=huge(0);cube(j)=0
        do i=0,5
          distance=(rgb(j)-levels(i))**2
          if(distance<best)then
            best=distance;cube(j)=i
          end if
        end do
      end do
      distance=sum((rgb-levels(cube))**2)
      gray=max(0,min(23,nint((real(sum(rgb),real32)/3.0_real32-8.0_real32)/10.0_real32)))
      gray_distance=sum((rgb-(8+10*gray))**2)
      color=16+36*cube(1)+6*cube(2)+cube(3)
      if(gray_distance<distance)color=232+gray
    else
      best=huge(0)
      do i=0,15
        distance=sum((rgb-palette(:,i))**2)
        if(distance<best)then
          best=distance;color=i
        end if
      end do
    end if
  end function terminal_color

  subroutine cell_style(x,row,fg,bg,glyph)
    integer,intent(in)::x,row
    integer,intent(out)::fg,bg
    character(len=1),intent(out)::glyph
    character(len=*),parameter::ramp=' .:-=+*#%@'
    integer::ty,by,top(3),bottom(3),rgb(3),back(3),index_value
    real(real32)::luminance
    logical::top_hit,bottom_hit
    ty=2*row-1;by=ty+1
    if(unicode_mode==1)then
      fg=terminal_color(pixels(x,ty));bg=terminal_color(pixels(x,by));glyph=achar(1);return
    end if
    top_hit=zbuffer(x,ty)<huge(1.0_real32)*.5_real32
    bottom_hit=zbuffer(x,by)<huge(1.0_real32)*.5_real32
    call unpack_rgb(background(x,ty),back)
    bg=terminal_color(pack_rgb(back(1),back(2),back(3)))
    fg=0;glyph=' '
    if(.not.(top_hit.or.bottom_hit))return
    call unpack_rgb(pixels(x,ty),top);call unpack_rgb(pixels(x,by),bottom)
    if(top_hit.and.bottom_hit)then
      rgb=(top+bottom)/2
    else if(top_hit)then
      rgb=top
    else
      rgb=bottom
    end if
    luminance=dot_product(real(rgb,real32),[.2126_real32,.7152_real32,.0722_real32])/255.0_real32
    index_value=max(2,min(len(ramp),1+nint(luminance*real(len(ramp)-1,real32))))
    glyph=ramp(index_value:index_value)
    fg=terminal_color(pack_rgb(rgb(1),rgb(2),rgb(3)))
  end subroutine cell_style

  subroutine write_snapshot()
    character(len=:),allocatable::line
    character(len=160)::label
    integer::row,x,fg,bg,n
    character(len=1)::glyph
    allocate(character(len=cols)::line)
    do row=1,rows
      line=repeat(' ',cols)
      if(hud_top==1.and.row==1)then
        label='Shape Lab | '//trim(mesh%name)//' | '//trim(theme%name)
        n=min(cols,len_trim(label));line(:n)=label(:n)
      else if(hud_bottom==1.and.row==rows)then
        label=trim(mesh%name)//' | shapes --help for controls'
        n=min(cols,len_trim(label));line(:n)=label(:n)
      else
        do x=1,cols
          call cell_style(x,row,fg,bg,glyph);line(x:x)=glyph
        end do
      end if
      write(output_unit,'(A)')trim(line)
    end do
  end subroutine write_snapshot

  subroutine append_cursor(pos,row,col)
    integer,intent(inout)::pos
    integer,intent(in)::row,col
    character(len=24)::tmp
    integer::n
    write(tmp,'(A,I0,A,I0,A)')ESC//'[',row,';',col,'H'
    n=len_trim(tmp); frame_text(pos:pos+n-1)=tmp(1:n); pos=pos+n
  end subroutine append_cursor

  subroutine append_pair(pos,fg,bg)
    integer,intent(inout)::pos
    integer,intent(in)::fg,bg
    integer::a(3),b(3)
    if(color_mode/=16777216)then
      call append_fg(pos,fg);call append_bg(pos,bg);return
    end if
    call unpack_rgb(fg,a);call unpack_rgb(bg,b)
    frame_text(pos:pos)=ESC;pos=pos+1;frame_text(pos:pos+5)='[38;2;';pos=pos+6
    call append_rgb_triplet(pos,a)
    frame_text(pos:pos+5)=';48;2;';pos=pos+6
    call append_rgb_triplet(pos,b)
    frame_text(pos:pos)='m';pos=pos+1
  end subroutine append_pair

  subroutine append_fg(pos,fg)
    integer,intent(inout)::pos
    integer,intent(in)::fg
    integer::a(3)
    if(color_mode/=16777216)then
      call append_indexed_color(pos,fg,.false.);return
    end if
    call unpack_rgb(fg,a);frame_text(pos:pos)=ESC;pos=pos+1;frame_text(pos:pos+5)='[38;2;';pos=pos+6
    call append_rgb_triplet(pos,a);frame_text(pos:pos)='m';pos=pos+1
  end subroutine append_fg

  subroutine append_bg(pos,bg)
    integer,intent(inout)::pos
    integer,intent(in)::bg
    integer::a(3)
    if(color_mode/=16777216)then
      call append_indexed_color(pos,bg,.true.);return
    end if
    call unpack_rgb(bg,a);frame_text(pos:pos)=ESC;pos=pos+1;frame_text(pos:pos+5)='[48;2;';pos=pos+6
    call append_rgb_triplet(pos,a);frame_text(pos:pos)='m';pos=pos+1
  end subroutine append_bg

  subroutine append_rgb_triplet(pos,a)
    integer,intent(inout)::pos
    integer,intent(in)::a(3)
    frame_text(pos:pos+2)=decimal3(a(1));pos=pos+3;frame_text(pos:pos)=';';pos=pos+1
    frame_text(pos:pos+2)=decimal3(a(2));pos=pos+3;frame_text(pos:pos)=';';pos=pos+1
    frame_text(pos:pos+2)=decimal3(a(3));pos=pos+3
  end subroutine append_rgb_triplet

  subroutine append_indexed_color(pos,color,is_background)
    integer,intent(inout)::pos
    integer,intent(in)::color
    logical,intent(in)::is_background
    character(len=24)::tmp
    integer::code,n
    if(color_mode==256)then
      write(tmp,'(A,I0,A,I0,A)')ESC//'[',merge(48,38,is_background),';5;',color,'m'
    else
      code=30+mod(color,8)
      if(color>=8)code=code+60
      if(is_background)code=code+10
      write(tmp,'(A,I0,A)')ESC//'[',code,'m'
    end if
    n=len_trim(tmp);frame_text(pos:pos+n-1)=tmp(:n);pos=pos+n
  end subroutine append_indexed_color

  subroutine append_glyph(pos,glyph)
    integer,intent(inout)::pos
    character(len=1),intent(in)::glyph
    if(glyph==achar(1))then
      frame_text(pos:pos+2)=UPPER_HALF;pos=pos+3
    else
      frame_text(pos:pos)=glyph;pos=pos+1
    end if
  end subroutine append_glyph

  subroutine write_hud()
    character(len=160)::identity,status,help_text
    character(len=12)::motion
    integer::bg(3),accent_bg(3),fg(3),muted(3),status_col,identity_col

    bg=min(theme%floor_rgb+[7,8,12],[32,34,42])
    accent_bg=min(nint(.28_real32*real(theme%base_a,real32)+.72_real32*real(bg,real32)),[86,86,94])
    fg=theme%edge_rgb
    muted=nint(.62_real32*real(fg,real32)+.38_real32*real(bg,real32))
    motion=merge('AUTO        ','PAUSED      ',auto_rotate)
    write(identity,'(I0,A,I0,4A)')shape_id,'/',max_shape,' ',trim(mesh%name),' / ',trim(theme%name)
    write(status,'(A,A,F3.1,A,F3.1)')trim(motion),'  ',auto_speed,'x  |  zoom ',camera_distance

    if(hud_top==1)then
      call paint_hud_row(1,bg)
      identity_col=2
      if(cols>=60)then
        call write_hud_text(1,2,' SHAPE LAB ',fg,accent_bg,.true.)
        identity_col=15
      end if
      call write_hud_text(1,identity_col,trim(identity),fg,bg,.true.)
      status_col=cols-len_trim(status)
      if(status_col>identity_col+len_trim(identity)+2) &
        call write_hud_text(1,status_col,trim(status),muted,bg,.false.)
    end if

    if(cols>=125)then
      help_text=' DRAG / ARROWS / WASD rotate   WHEEL / ZX zoom   N/P shape   T theme'// &
                '   SPACE pause   R reset   E edges   H hide   Q quit'
    else if(cols>=78)then
      help_text=' DRAG/WASD rotate  WHEEL/ZX zoom  N/P shape  T theme  SPACE pause  H UI  Q quit'
    else if(cols>=47)then
      help_text=' N/P shape  T theme  SPACE pause  H UI  Q quit'
    else if(cols>=25)then
      help_text=' Q quit  N/P shape  T theme'
    else
      help_text=' Q quit  N/P shape'
    end if
    if(hud_bottom==1)then
      call paint_hud_row(rows,bg)
      call write_hud_text(rows,1,trim(help_text),muted,bg,.false.)
    end if
    write(output_unit,'(A)',advance='no')ESC//'[0m'
  end subroutine write_hud

  subroutine paint_hud_row(row,bg)
    integer,intent(in)::row,bg(3)
    character(len=:),allocatable::blank
    allocate(character(len=cols)::blank)
    blank(:)=' '
    call move_terminal_cursor(row,1)
    call set_hud_style([232,238,247],bg,.false.)
    write(output_unit,'(A)',advance='no')blank
  end subroutine paint_hud_row

  subroutine write_hud_text(row,col,text_value,fg,bg,bold)
    integer,intent(in)::row,col,fg(3),bg(3)
    character(len=*),intent(in)::text_value
    logical,intent(in)::bold
    integer::n
    if(col<1 .or. col>cols)return
    n=min(len_trim(text_value),cols-col+1)
    if(n<=0)return
    call move_terminal_cursor(row,col)
    call set_hud_style(fg,bg,bold)
    write(output_unit,'(A)',advance='no')text_value(1:n)
  end subroutine write_hud_text

  subroutine move_terminal_cursor(row,col)
    integer,intent(in)::row,col
    write(output_unit,'(A,I0,A,I0,A)',advance='no')ESC//'[',row,';',col,'H'
  end subroutine move_terminal_cursor

  subroutine set_hud_style(fg,bg,bold)
    integer,intent(in)::fg(3),bg(3)
    logical,intent(in)::bold
    integer::a,b
    write(output_unit,'(A)',advance='no')ESC//'[0m'
    if(bold)write(output_unit,'(A)',advance='no')ESC//'[1m'
    if(color_mode==16777216)then
      write(output_unit,'(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A)',advance='no') &
        ESC//'[38;2;',fg(1),';',fg(2),';',fg(3),';48;2;',bg(1),';',bg(2),';',bg(3),'m'
    else if(color_mode/=0)then
      a=terminal_color(pack_rgb(fg(1),fg(2),fg(3)));b=terminal_color(pack_rgb(bg(1),bg(2),bg(3)))
      if(color_mode==256)then
        write(output_unit,'(A,I0,A,I0,A)',advance='no')ESC//'[38;5;',a,';48;5;',b,'m'
      else
        a=30+mod(a,8)+merge(60,0,a>=8);b=40+mod(b,8)+merge(60,0,b>=8)
        write(output_unit,'(A,I0,A,I0,A)',advance='no')ESC//'[',a,';',b,'m'
      end if
    end if
  end subroutine set_hud_style

  subroutine poll_input(frame_dt)
    real(real32),intent(in)::frame_dt
    integer(c_int)::nread
    integer::i,batch
    character(len=1)::ch
    input_age=input_age+frame_dt
    if(input_age>0.5_real32.and.input_state>=1.and.input_state<=4)input_state=0
    do batch=1,8
      nread=terminal_read(input_bytes,int(size(input_bytes),c_int))
      if(nread<0)stop_requested=1
      if(nread<=0)exit
      do i=1,int(nread)
        ch=achar(iachar(input_bytes(i)))
        call feed_input_char(ch,frame_dt)
      end do
      if(nread<int(size(input_bytes),c_int))exit
    end do
  end subroutine poll_input

  subroutine feed_input_char(ch,frame_dt)
    character(len=1),intent(in)::ch
    real(real32),intent(in)::frame_dt
    integer::code
    code=iachar(ch);input_age=0.0_real32
    select case(input_state)
    case(0)
      if(iachar(ch)==27)then
        input_state=1
      else
        call handle_key(ch)
      end if
    case(1)
      mouse_len=0;mouse_text=''
      select case(ch)
      case('[');input_state=2
      case('O');input_state=4
      case(']','P','^','_');input_state=5
      case default;input_state=0
      end select
    case(2,4)
      if(ch=='<'.and.mouse_len==0)then
        input_state=3;mouse_len=0;mouse_text=''
      else if(code>=64.and.code<=126)then
        call handle_arrow_key(ch)
        input_state=0
        if(ch=='~'.and.trim(mouse_text)=='200')input_state=8
      else if(code>=32.and.code<=63.and.mouse_len<len(mouse_text))then
        mouse_len=mouse_len+1;mouse_text(mouse_len:mouse_len)=ch
      else if(code==27)then
        input_state=1
      else
        input_state=7
      end if
    case(3)
      if(ch=='M'.or.ch=='m')then
        call handle_mouse(mouse_text(1:max(1,mouse_len)),ch=='m',frame_dt)
        input_state=0;mouse_len=0
      else if(mouse_len<len(mouse_text).and.((ch>='0'.and.ch<='9').or.ch==';'))then
        mouse_len=mouse_len+1;mouse_text(mouse_len:mouse_len)=ch
      else
        input_state=7;mouse_len=0
      end if
    case(5)
      if(code==7)input_state=0
      if(code==27)input_state=6
    case(6)
      input_state=5
      if(ch==achar(92))input_state=0
    case(7)
      if(code>=64.and.code<=126)input_state=0
      if(code==27)input_state=1
    case(8)
      if(code==27)input_state=9
    case(9)
      input_state=8
      if(ch=='[')then
        input_state=10;mouse_len=0;mouse_text=''
      end if
    case(10)
      if(code>=64.and.code<=126)then
        input_state=8
        if(ch=='~'.and.trim(mouse_text)=='201')input_state=0
      else if(mouse_len<len(mouse_text))then
        mouse_len=mouse_len+1;mouse_text(mouse_len:mouse_len)=ch
      else
        input_state=8
      end if
    end select
  end subroutine feed_input_char

  subroutine handle_key(ch)
    character(len=1),intent(in)::ch
    integer::v
    select case(ch)
    case('q','Q',achar(3),achar(4))
      stop_requested=1
    case(' ')
      auto_rotate=.not.auto_rotate; force_redraw=.true.
    case('t','T')
      theme_id=1+mod(theme_id,max_theme); call load_theme(theme_id,theme); call rebuild_background()
    case('r','R')
      call rotation_matrix(-.34_real32,.62_real32,.08_real32,orientation)
      camera_distance=5.0_real32;angular_velocity=0.0_real32
      interaction_quiet=.8_real32;force_redraw=.true.
    case('h','H')
      show_hud=.not.show_hud;call update_layout();force_redraw=.true.
    case('e','E')
      draw_edges=.not.draw_edges;force_redraw=.true.
    case('[','-')
      auto_speed=max(.15_real32,auto_speed-.15_real32)
    case(']','+')
      auto_speed=min(3.0_real32,auto_speed+.15_real32)
    case('a','A')
      call apply_orientation_delta([0.0_real32,1.0_real32,0.0_real32],.12_real32)
      angular_velocity=0.0_real32;interaction_quiet=.5_real32
    case('d','D')
      call apply_orientation_delta([0.0_real32,1.0_real32,0.0_real32],-.12_real32)
      angular_velocity=0.0_real32;interaction_quiet=.5_real32
    case('w','W')
      call apply_orientation_delta([1.0_real32,0.0_real32,0.0_real32],.10_real32)
      angular_velocity=0.0_real32;interaction_quiet=.5_real32
    case('s','S')
      call apply_orientation_delta([1.0_real32,0.0_real32,0.0_real32],-.10_real32)
      angular_velocity=0.0_real32;interaction_quiet=.5_real32
    case('z','Z')
      camera_distance=max(3.15_real32,camera_distance-.25_real32); interaction_quiet=.5_real32
    case('x','X')
      camera_distance=min(9.0_real32,camera_distance+.25_real32); interaction_quiet=.5_real32
    case('0','1','2','3','4','5','6','7','8','9')
      read(ch,*)v
      if(v==0)v=10
      shape_id=v;call load_shape(shape_id,mesh);interaction_quiet=.45_real32;force_redraw=.true.
    case('n','N',achar(9))
      shape_id=1+mod(shape_id,max_shape);call load_shape(shape_id,mesh)
      interaction_quiet=.45_real32;force_redraw=.true.
    case('p','P')
      shape_id=1+modulo(shape_id-2,max_shape);call load_shape(shape_id,mesh)
      interaction_quiet=.45_real32;force_redraw=.true.
    end select
  end subroutine handle_key

  subroutine handle_arrow_key(ch)
    character(len=1),intent(in)::ch
    select case(ch)
    case('A')
      call apply_orientation_delta([1.0_real32,0.0_real32,0.0_real32],.10_real32)
    case('B')
      call apply_orientation_delta([1.0_real32,0.0_real32,0.0_real32],-.10_real32)
    case('C')
      call apply_orientation_delta([0.0_real32,1.0_real32,0.0_real32],-.12_real32)
    case('D')
      call apply_orientation_delta([0.0_real32,1.0_real32,0.0_real32],.12_real32)
    case default
      return
    end select
    angular_velocity=0.0_real32;interaction_quiet=.5_real32
  end subroutine handle_arrow_key

  subroutine handle_mouse(text,released,frame_dt)
    character(len=*),intent(in)::text
    logical,intent(in)::released
    real(real32),intent(in)::frame_dt
    character(len=64)::tmp
    integer::i,ios,b,x,y,base,dx,dy
    real(real32)::current_vector(3),axis(3),axis_length,angle,dot_value,speed
    tmp='';tmp(1:min(len_trim(text),len(tmp)))=text(1:min(len_trim(text),len(tmp)))
    do i=1,len_trim(tmp);if(tmp(i:i)==';')tmp(i:i)=' ';end do
    read(tmp,*,iostat=ios)b,x,y
    if(ios/=0)return
    if(b<0.or.b>255)return
    if(x<1.or.x>cols.or.y<1.or.y>rows)then
      if(released)dragging=.false.
      return
    end if
    if(x>=1 .and. x<=cols .and. y>=1 .and. y<=rows)then
      mouse_present=.true.
    end if
    base=iand(b,3)
    if(iand(b,64)/=0)then
      if(iand(b,1)==0)then
        camera_distance=max(3.15_real32,camera_distance-.28_real32)
      else
        camera_distance=min(9.0_real32,camera_distance+.28_real32)
      end if
      mouse_x=x;mouse_y=y
      interaction_quiet=.7_real32;force_redraw=.true.;return
    end if
    if(released)then
      mouse_x=x;mouse_y=y
      dragging=.false.;interaction_quiet=.85_real32;return
    end if
    if(iand(b,32)/=0 .and. dragging)then
      dx=x-mouse_x;dy=y-mouse_y
      if(abs(dx)<=40.and.abs(dy)<=25)then
        call trackball_vector(x,y,current_vector)
        axis=cross3(drag_vector,current_vector)
        axis_length=sqrt(dot_product(axis,axis))
        dot_value=clamp_real(dot_product(drag_vector,current_vector),-1.0_real32,1.0_real32)
        if(axis_length>1.0e-5_real32)then
          angle=atan2(axis_length,dot_value)
          axis=axis/axis_length
          call apply_orientation_delta(axis,angle)
          speed=min(8.0_real32,angle/max(frame_dt,.008_real32))
          angular_velocity=axis*speed
        end if
        drag_vector=current_vector
      end if
      mouse_x=x;mouse_y=y;interaction_quiet=.85_real32;return
    end if
    if(iand(b,32)/=0)then
      mouse_x=x;mouse_y=y;return
    end if
    if(base==0)then
      if(y>hud_top.and.y<=rows-hud_bottom)then
        dragging=.true.;mouse_x=x;mouse_y=y;angular_velocity=0.0_real32
        call trackball_vector(x,y,drag_vector)
        interaction_quiet=.85_real32
      end if
    end if
  end subroutine handle_mouse

  pure function cross3(a,b) result(c)
    real(real32),intent(in)::a(3),b(3)
    real(real32)::c(3)
    c=[a(2)*b(3)-a(3)*b(2),a(3)*b(1)-a(1)*b(3),a(1)*b(2)-a(2)*b(1)]
  end function cross3

  pure function normalize3(v) result(n)
    real(real32),intent(in)::v(3)
    real(real32)::n(3),ls
    ls=dot_product(v,v);if(ls<=1.0e-12_real32)then;n=0.0_real32;else;n=v/sqrt(ls);end if
  end function normalize3

  pure function clamp_vec(v,lo,hi) result(r)
    real(real32),intent(in)::v(3),lo,hi
    real(real32)::r(3);r=min(max(v,lo),hi)
  end function clamp_vec

  pure real(real32) function clamp01(v)
    real(real32),intent(in)::v;clamp01=min(max(v,0.0_real32),1.0_real32)
  end function clamp01

  pure real(real32) function clamp_real(v,lo,hi)
    real(real32),intent(in)::v,lo,hi;clamp_real=min(max(v,lo),hi)
  end function clamp_real

  pure integer function clamp255(v)
    real(real32),intent(in)::v;clamp255=min(max(nint(v),0),255)
  end function clamp255

  pure real(real32) function edge_function(ax,ay,bx,by,cx,cy)
    real(real32),intent(in)::ax,ay,bx,by,cx,cy
    edge_function=(cx-ax)*(by-ay)-(cy-ay)*(bx-ax)
  end function edge_function

  pure integer(int32) function pack_rgb(r,g,b)
    integer,intent(in)::r,g,b
    pack_rgb=ishft(int(iand(r,255),int32),16)+ishft(int(iand(g,255),int32),8)+int(iand(b,255),int32)
  end function pack_rgb

  pure subroutine unpack_rgb(packed,rgb)
    integer(int32),intent(in)::packed
    integer,intent(out)::rgb(3)
    rgb(1)=int(ibits(packed,16,8));rgb(2)=int(ibits(packed,8,8));rgb(3)=int(ibits(packed,0,8))
  end subroutine unpack_rgb

end program fortran_shape_lab

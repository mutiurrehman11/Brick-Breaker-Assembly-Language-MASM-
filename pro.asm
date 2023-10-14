;                                         ---- Brick Breaker-----
;                            ----By Muti Ur Rehman And Huzaifa Tahir Rathore----

Brick struct
	xaxis dw 0
	yaxis dw 0
	open dw 0
Brick ends

.model small
.stack 100h
.data

;Brick Breaker Game Data Declaration



;Main Menu
		Na db 'BRICK BREAKER$'
		start db 'Start$'
		inst db 'Instruction$'
		exit1 db 'Exit$'

;Instructions Menu
		inst1 db '1: Space To Start$'
		inst2 db '2: Space To Pause$'
		inst3 db '3: Good Luck$'
		inst4 db '<Press Any Key To Continue>$'

;Level 1

	;Menu
		lv1 db 'LEVEL 1$' 
	;Bricks 
		BR1 Brick <45,25,1>
		BR2 Brick <85,25,1>
		BR3 Brick <125,25,1>
		BR4 Brick <165,25,1>
		BR5 Brick <205,25,1>
		BR6 Brick <245,25,1>
		BR7 Brick <45,45,1>
		BR8 Brick <85,45,1>
		BR9 Brick <125,45,1>
		BR10 Brick <165,45,1>
		BR11 Brick <205,45,1>
		BR12 Brick <245,45,1>
		BR13 Brick <45,65,1>
		BR14 Brick <85,65,1>
		BR15 Brick <125,65,1>
		BR16 Brick <165,65,1>
		BR17 Brick <205,65,1>
		BR18 Brick <245,65,1>
		
	;Boundary
		b_x dw 0
		b_y dw 0
		e_x dw 0
		e_y dw 0 
		
		boundaryEnd dw 250
		boundaryStart dw 30
	
	;Color
		color db 7
	
	;Ball
		ballY dw 163
		ballX dw 158
		ballLeft db 1
		ballUp db 1
		
	;Striker
		strikerX dw 140
		strikerY dw 170
		stlength dw 40
		
	;Scores
		score db 'Score: $'
		scoreCount dw 0
		lvScore dw 18
		
	;Ball Speed
		speed dw 2
		
	;Lives
		lives db '              Lives: '
		livesCount db 51
		ending db ' $'
		begin db 0
		
	;Temporary
		t1 dw ?
		t2 dw ?
		chec dw 0
		levchec dw 0
	;Fixed Brick
		fixy dw 0
		
		
;Level 2
		
	;Menu
		lvv2 db 'LEVEL 1 Completed$'
		lv2 db 'LEVEL 2$' 

;Level 3

	;Menu
		lvv3 db 'LEVEL 2 Completed$'
		lv3 db 'LEVEL 3$'
		
		fixed dw 0
		
	;Pause Menu
		pau db 'Pause Menu$'
		pau1 db 'Exit$'
		
	;Input Name
		strname db 'ENTER NAME$'
		str1 db 40 dup(' ')
		strlen dw 0
		
	;File Handling
	
		fname db "MYFILE.txt",0
		handle dw ?
		sc1 db 0
		sc2 db 0
	; Win Menu
		Gam db 'Game Won$'
		
.code

jmp main

DrawLivesScores proc uses dx ax
                 
    mov dh, 23 ;row
    mov dl, 5 ;col
    mov ah, 2 
    int 10h
    
    mov dx, offset score
    mov ah, 9
    int 21h
    
    call printScore
    
    mov dx, offset lives
    mov ah,9
    int 21h  

    ret
DrawLivesScores endp

printScore proc uses ax bx cx dx

    mov cx,0
 
    mov ax,scoreCount
    ll:
    mov bx,10
    mov dx,0
    div bx
    push dx
    inc cx
    cmp ax,0
    jne ll
    
    l2:
		pop dx
		mov ah,2
		add dl,48
		int 21h
    loop l2
    
    ret
printScore endp

checkKeyboard proc
    mov     ah,     1h
    int     16h         ; check keypress
    jz      noInput     ; no keypress
    mov     ah,     0h
    int     16h
    cmp     ah,     4Dh
    je      rightKey
    cmp     ah,     4Bh
    je      leftKey
    cmp     al,     27D
    je      exit
    cmp     ax,     3920h;space to begin
    je      beg
    jne     noInput
    
    beg:
	cmp begin,1
	jne begg
	call PauseMenu
	jmp noInput
	begg:
    mov begin,1
    
    noInput:
    ret  
	
    rightKey:     
    mov bx, boundaryEnd
    cmp     strikerX, bx ;max right limit
    jg      noInput
	mov color, 0
    call drawStriker 
    add     strikerX, 5
	mov color, 7
    call drawStriker
    cmp begin,0
    jz moveBallRight
    jmp     noInput
    
    
    leftKey:   
    mov bx, boundaryStart                            
    cmp     strikerX, bx ;max left limit
    jl      noInput
	mov color, 0
    call drawStriker
    sub     strikerX, 5
	mov color, 7
    call drawStriker
    cmp begin,0
    jz moveBallLeft
    jmp     noInput
    
    
    moveBallLeft:
	mov color, 0
    call drawBall
    sub     ballX, 5
	mov color, 3
    call drawBall
    jmp     noInput
    
    
    moveBallRight:
	mov color, 0
    call drawBall
    add     ballX, 5
	mov color, 3
    call drawBall
    jmp     noInput

checkKeyboard endp

CollisionStriker proc uses ax bx cx dx  
    
    mov dx, ballY
    cmp dx, 165 ; striker surface check
    jl ender
    cmp dx, 170 ; striker missed
    jg fail 
    
    
    
    mov cx,strikerX   
    mov ax, ballX   
    cmp ax, cx  
    jl ender
    add cx , stlength
    cmp ax, cx
    jg ender
    
    mov ballUp, 1
    jmp ender
    
    
    fail:
    mov begin,0 
    dec livesCount
    cmp livesCount,48
    je over
    push ax
    push bx
    push cx
    push dx
    
    mov color,0
    call drawBall
    
    mov ax, strikerX
    mov ballX,ax
    add ballX,18
    
    mov ballY,  163
    
	mov color,3
    call drawBall
    mov ballUp, 1     ;monis
    mov ballLeft,0
    
    
    
    pop dx
    pop cx
    pop bx
    pop ax
    call Clear_Down
    call DrawLivesScores
    jmp ender
    
    over:             
    call DrawLivesScores
    mov ah,4ch
    int 21h 
                  
    ender:  
    ret
CollisionStriker endp

Collisionwall proc     
    
    mov bx, ballX
    mov cx, ballY
    
    checkLeftRight:
    cmp bx, 25; max left
    jl goRight
    cmp bx, 290; Max Right
    jg goLeft
    jmp checkUpDown
    goRight:
    mov ballLeft, 0 
    jmp checkUpDown;
    goLeft:
    mov ballLeft, 1
    checkUpDown:
    
    cmp cx, 13;max top
    jl goDown
    cmp cx, 184;max bottom
    jg goUp
    
    jmp noInput
    goUp:                                            
    mov ballUp,1
    jmp noInput
    goDown: 
    mov ballUp, 0
  
	noInput:
    ret
Collisionwall endp

baller proc  
	
	mov color,0
    call drawBall  
    
	mov bx,ballX 
	cmp ballLeft, 1
	je Left
	jne Right
	
	Left:   
	sub bx, speed 
	jmp P2 
	Right:   
	add bx, speed
	
	P2:
	mov ballX,  bx
	mov bx, ballY
	cmp ballUp, 1   
	je Up
	jne Down
	Up:
    sub bx, speed
	jmp P3
	Down:
    add bx, speed
	P3:
    mov ballY,  bx
	
    mov color,3
    call drawBall
    ender1:
	ret
baller endp   

RemoveBrick proc uses ax bx cx dx 
       
    mov b_x, ax
    mov color, 0  
    mov ax, bx
    mov bx, b_x
    
    add bx, 30
    
    mov e_x,bx
    
    mov b_y, ax 
    
    mov bx,b_y
    
    add bx,7
    mov e_y,bx
     
    call draw 
	
    ret
RemoveBrick endp

DestroyBrick proc uses ax bx
    mov ax, t1
    mov bx, t2
    call RemoveBrick    
	inc scoreCount
    call DrawLivesScores
	ret
DestroyBrick endp

switcher proc
    cmp ballUp, 1
    je DownT
    jne UpT
    UpT:
    inc ballUp
    ret
    DownT:
    dec ballUp
    ret
switcher endp

BrickCollision proc uses ax bx cx dx
  
    mov ax, ballY
    mov bx, ballX
    mov t1,cx
	mov t2,dx
	add dx,8
    cmp dx, ballY
    jl copper
    sub dx, 16
    
    cmp ballY, dx
    jl copper
    
    mov dx, t1 
    sub dx,4
    cmp ballX, dx
    jl copper
    add dx, 34
    cmp dx, ballX
    jl copper
    
    call switcher
    call DestroyBrick
	mov chec,1
    mov t2, 300
	mov bx,lvScore
    cmp scoreCount, bx
    jne copper
	cmp levchec,2
	je endGame
	cmp levchec,0
	je Level2Menu
	jmp Level3Menu
    copper:                   
    ret
	
BrickCollision endp

sleep proc

	mov cx,11111111111111111b 
	l:
	loop l
	ret
sleep endp

Normalize proc
	
	cmp BR1.open,0
    je cch1
    mov ax, BR1.xaxis
    mov bx, BR1.yaxis
    call AddBrick
	
	cch1:
	cmp BR2.open,0
    je cch2
    mov ax, BR2.xaxis
    mov bx, BR2.yaxis
    call AddBrick
	
	cch2:
	
	cmp BR3.open,0
    je cch3
    mov ax, BR3.xaxis
    mov bx, BR3.yaxis
    call AddBrick
	
	cch3:
	
	cmp BR4.open,0
    je cch4
    mov ax, BR4.xaxis
    mov bx, BR4.yaxis
    call AddBrick
	
	cch4:
	
	cmp BR5.open,0
    je cch5
    mov ax, BR5.xaxis
    mov bx, BR5.yaxis
    call AddBrick
	
	cch5:
	
	cmp BR6.open,0
    je cch6
    mov ax, BR6.xaxis
    mov bx, BR6.yaxis
    call AddBrick
	
	cch6:
	
	cmp BR7.open,0
    je cch7
    mov ax, BR7.xaxis
    mov bx, BR7.yaxis
    call AddBrick
	
	cch7:
	
	cmp BR8.open,0
    je cch8
    mov ax, BR8.xaxis
    mov bx, BR8.yaxis
    call AddBrick
	
	cch8:
	
	cmp BR9.open,0
    je cch9
    mov ax, BR9.xaxis
    mov bx, BR9.yaxis
    call AddBrick
	
	cch9:
	
	cmp BR10.open,0
    je cch10
    mov ax, BR10.xaxis
    mov bx, BR10.yaxis
    call AddBrick
	
	cch10:
	
	cmp BR11.open,0
    je cch11
    mov ax, BR11.xaxis
    mov bx, BR11.yaxis
    call AddBrick
	
	cch11:
	
	cmp BR12.open,0
    je cch12
    mov ax, BR12.xaxis
    mov bx, BR12.yaxis
    call AddBrick
	
	cch12:
	
	cmp BR13.open,0
    je cch13
    mov ax, BR13.xaxis
    mov bx, BR13.yaxis
    call AddBrick
	
	cch13:
	
	cmp BR14.open,0
    je cch14
    mov ax, BR14.xaxis
    mov bx, BR14.yaxis
    call AddBrick
	
	cch14:
	
	cmp BR15.open,0
    je cch15
    mov ax, BR15.xaxis
    mov bx, BR15.yaxis
    call AddBrick
	
	cch15:
	
	cmp BR16.open,0
    je cch16
    mov ax, BR16.xaxis
    mov bx, BR16.yaxis
    call AddBrick
	
	cch16:
	
	cmp BR17.open,0
    je cch17
    mov ax, BR17.xaxis
    mov bx, BR17.yaxis
    call AddBrick
	
	cch17:
	
	cmp BR18.open,0
    je cch18
    mov ax, BR18.xaxis
    mov bx, BR18.yaxis
    call AddBrick
	
	cch18:
		
	ret
Normalize endp

draw proc uses ax cx dx
     
    mov dx,b_y
    mov cx,b_x
    mov ah,0ch
    mov al,color
    col:
    inc cx
    int 10h
    cmp cx,e_x
    jne col

    mov cx,b_x
    inc dx
    cmp dx,e_y
    jne col 
	
    ret
draw endp

AddBrick proc uses ax bx
  
    mov b_x, ax        ;ax=xaxis   b_x=xaxis
    mov ax, bx		   ;ax=yaxis   bx=yaxis
    mov bx, b_x
    add bx, 30
    
    mov e_x,bx
    mov b_y, ax 
    mov bx,b_y
                    
    add bx,7
    mov e_y,bx
     
    call draw
	
    ret
AddBrick endp

drawball proc uses bx

    mov bx, ballX
    mov b_x, bx
    add bx, 4 
	
    mov e_x,bx
    mov bx, ballY
    mov b_y,bx
    add bx, 4
    mov e_y,bx
    
    call draw
ret
drawball endp

drawStriker proc uses bx cx
        
    mov bx, strikerX
    mov cx, strikerY   
    mov b_x,bx
    add bx, stlength
    mov e_x,bx
    mov b_y,cx
    mov e_y,175
    call draw

    ret
drawStriker endp

gameLoop proc

	repeat1:
	CALL checkKeyboard
    cmp begin,1
	jne repeat1
   
   
   
   call Collisionwall
   call CollisionStriker 
   
   cmp BR1.open,0
   je ch1
   mov cx, BR1.xaxis
   mov dx,BR1.yaxis
   call BrickCollision
   cmp chec,1
   jne ch1
   sub BR1.open,1
   mov chec,0
   
   call Collisionwall
   call CollisionStriker 
   
   ch1:
   cmp BR2.open,0
   je ch2
   mov cx, BR2.xaxis
   mov dx,BR2.yaxis
   call BrickCollision
   cmp chec,1
   jne ch2
   sub BR2.open,1
   mov chec,0
   jmp ch18
   
   ch2:
   cmp BR3.open,0
   je ch3
   mov cx, BR3.xaxis
   mov dx,BR3.yaxis
   call BrickCollision
   cmp chec,1
   jne ch3
   sub BR3.open,1
   mov chec,0
   jmp ch18
   
   ch3:
   cmp BR4.open,0
   je ch4
   mov cx, BR4.xaxis
   mov dx,BR4.yaxis
   call BrickCollision
   cmp chec,1
   jne ch4
   sub BR4.open,1
   mov chec,0
   jmp ch18
   
   ch4:
   cmp BR5.open,0
   je ch5
   mov cx, BR5.xaxis
   mov dx,BR5.yaxis
   call BrickCollision
   cmp chec,1
   jne ch5
   sub BR5.open,1
   mov chec,0
   jmp ch18
   
   ch5:
   cmp BR6.open,0
   je ch6
   mov cx, BR6.xaxis
   mov dx,BR6.yaxis
   call BrickCollision
   cmp chec,1
   jne ch6
   sub BR6.open,1
   mov chec,0
   jmp ch18
   
   ch6:
   cmp BR7.open,0
   je ch7
   mov cx, BR7.xaxis
   mov dx,BR7.yaxis
   call BrickCollision
   cmp chec,1
   jne ch7
   sub BR7.open,1
   mov chec,0
   jmp ch18
   
   ch7:
   cmp BR8.open,0
   je ch8
   mov cx, BR8.xaxis
   mov dx,BR8.yaxis
   call BrickCollision
   cmp chec,1
   jne ch8
   sub BR8.open,1
   mov chec,0
   jmp ch18
   
   ch8:
   cmp BR9.open,0
   je ch9
   mov cx, BR9.xaxis
   mov dx,BR9.yaxis
   call BrickCollision
   cmp chec,1
   jne ch9
   sub BR9.open,1
   mov chec,0
   jmp ch18
   
   ch9:
   cmp BR10.open,0
   je ch10
   mov cx, BR10.xaxis
   mov dx,BR10.yaxis
   call BrickCollision
   cmp chec,1
   jne ch10
   sub BR10.open,1
   mov chec,0
   jmp ch18
   
   ch10:
   cmp BR11.open,0
   je ch11
   mov cx, BR11.xaxis
   mov dx,BR11.yaxis
   call BrickCollision
   cmp chec,1
   jne ch11
	   sub BR11.open,1
	   mov chec,0
	   jmp ch18
   
   ch11:
   cmp BR12.open,0
   je ch12
   mov cx, BR12.xaxis
   mov dx,BR12.yaxis
   call BrickCollision
   cmp chec,1
   jne ch12
	sub BR12.open,1
	mov chec,0
	jmp ch18
   
   ch12:
   cmp BR13.open,0
   je ch13
   mov cx, BR13.xaxis
   mov dx,BR13.yaxis
   call BrickCollision
   cmp chec,1
   jne ch13
   sub BR13.open,1
   mov chec,0
   jmp ch18
   
   ch13:
   cmp BR14.open,0
   je ch14
   mov cx, BR14.xaxis
   mov dx,BR14.yaxis
   call BrickCollision
   cmp chec,1
   jne ch14
   sub BR14.open,1
   mov chec,0
   jmp ch18
   
   ch14:
   cmp BR15.open,0
   je ch15
   mov cx, BR15.xaxis
   mov dx,BR15.yaxis
   call BrickCollision
   cmp chec,1
   jne ch15
   sub BR15.open,1
   mov chec,0
   jmp ch18
   
   ch15:
   cmp BR16.open,0
   je ch16
   mov cx, BR16.xaxis
   mov dx,BR16.yaxis
   call BrickCollision
   cmp chec,1
   jne ch16
   sub BR16.open,1
   mov chec,0
   jmp ch18
   
   ch16:
   cmp BR17.open,0
   je ch17
   mov cx, BR17.xaxis
   mov dx,BR17.yaxis
   call BrickCollision
   cmp chec,1
   jne ch17
   sub BR17.open,1
   mov chec,0
   jmp ch18
   
   ch17:
   cmp BR18.open,0
   je ch18
   mov cx, BR18.xaxis
   mov dx,BR18.yaxis
   call BrickCollision
   cmp chec,1
   jne ch18
	sub BR18.open,1
	mov chec,0
   
   ch18:
   mov color,7
   call Normalize
   mov color,4
   ;------TOP------------
			mov b_x,20
			mov e_x,300
			mov b_y,5
			mov e_y,8
			call draw
			;------RIGHT------------
			mov b_x,297
			mov e_x,300
			mov b_y,7
			mov e_y,180
			call draw
			;------LEFT------------
			mov b_x,20
			mov e_x,23
			mov b_y,7
			mov e_y,180
			call draw
			;------BOTTOM------------
			mov b_x,20
			mov e_x,300
			mov b_y,177
			mov e_y,180
			call draw 
   
   CALL baller  
   CALL sleep
   JMP     gameLoop 
	
	ret
	
gameLoop endp

exit:
    mov ah, 4ch
    int 21h
	
Clear_Screen proc
	
	mov color,0
	mov b_x,23
	mov e_x,297
	mov b_y,9
	mov e_y,177
	call draw
	ret
Clear_Screen endp

Clear_Down proc
	
	mov color,0
	mov b_x,23
	mov e_x,320
	mov b_y,181
	mov e_y,200
	call draw
	ret
Clear_Down endp

Instructions_Menu proc
			
			mov dh, 4 ;row
			mov dl, 5 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset inst1
			mov ah, 9
			int 21h
			
			mov dh, 8 ;row
			mov dl, 5 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset inst2
			mov ah, 9
			int 21h
			
			mov dh, 12 ;row
			mov dl, 5 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset inst3
			mov ah, 9
			int 21h
			
			mov dh, 16 ;row
			mov dl, 5 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset inst4
			mov ah, 9
			int 21h
			
			lll1:
			mov ah,1
			int 16h
			jz lll1
			
			ret
Instructions_Menu endp
	
MAINMENU proc
	;Selection Boxes
			MAINMENUToken:
			mov color,7
				;-------Main Box-------
				mov b_x,97
				mov e_x,213
				mov b_y,11
				mov e_y,29
				call draw
				;-------Box1-------
				mov b_x,35
				mov e_x,130
				mov b_y,43
				mov e_y,61
				call draw
				
				;-------Box2-------
				mov b_x,35
				mov e_x,130
				mov b_y,74
				mov e_y,92
				call draw
				
				;-------Box3-------
				mov b_x,35
				mov e_x,130
				mov b_y,105
				mov e_y,123
				call draw
				
				
			mov dh, 2 
			mov dl, 13
			mov ah, 2 
			int 10h
			
			mov dx, offset Na
			mov ah, 9
			int 21h
			
			mov dh, 6 ;row
			mov dl, 5 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset start
			mov ah, 9
			int 21h
			
			mov dh, 10 ;row
			mov dl, 5 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset inst
			mov ah, 9
			int 21h
			
			mov dh, 14 ;row
			mov dl, 5 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset exit1
			mov ah, 9
			int 21h
			
			ll1:
			mov ah, 0h
			int 16h 
			mov ah, 1h
			int 16h 
			cmp al,49
			je ll4
			cmp al,50
			je ll3
			cmp al,51
			je ll2
			jmp ll1
			
			
			
			ll3:
			call Clear_Screen
			call Instructions_Menu
			call Clear_Screen
			jmp MAINMENUToken
			ll2:
			mov ah,4ch
			int 21h
			
			ll4:
	ret
MAINMENU endp

Level1Menu proc
	
			mov dh, 8 ;row
			mov dl, 16 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset lv1
			mov ah, 9
			int 21h
			
			mov dh, 12 ;row
			mov dl, 6 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset inst4
			mov ah, 9
			int 21h
			
			llvl1:
			mov ah,1
			int 16h
			jz llvl1
	
  ret
Level1Menu endp

;--------------------------------Normalization---------------------------

Level1_Normalize proc uses ax bx

	mov BR1.open,1
	mov BR1.xaxis,45
	mov BR1.yaxis,25
	mov BR2.open,1
	mov BR2.xaxis,85
	mov BR2.yaxis,25
	mov BR3.open,1
	mov BR3.xaxis,125
	mov BR3.yaxis,25
	mov BR4.open,1
	mov BR4.xaxis,165
	mov BR4.yaxis,25
	mov BR5.open,1
	mov BR5.xaxis,205
	mov BR5.yaxis,25
	mov BR6.open,1
	mov BR6.xaxis,245
	mov BR6.yaxis,25
	mov BR7.open,1
	mov BR7.xaxis,45
	mov BR7.yaxis,45
	mov BR8.open,1
	mov BR8.xaxis,85
	mov BR8.yaxis,45
	mov BR9.open,1
	mov BR9.xaxis,125
	mov BR9.yaxis,45
	mov BR10.open,1
	mov BR10.xaxis,165
	mov BR10.yaxis,45
	mov BR11.open,1
	mov BR11.xaxis,205
	mov BR11.yaxis,45
	mov BR12.open,1
	mov BR12.xaxis,245
	mov BR12.yaxis,45
	mov BR13.open,1
	mov BR13.xaxis,45
	mov BR13.yaxis,65
	mov BR14.open,1
	mov BR14.xaxis,85
	mov BR14.yaxis,65
	mov BR15.open,1
	mov BR15.xaxis,125
	mov BR15.yaxis,65
	mov BR16.open,1
	mov BR16.xaxis,165
	mov BR16.yaxis,65
	mov BR17.open,1
	mov BR17.xaxis,205
	mov BR17.yaxis,65
	mov BR18.open,1
	mov BR18.xaxis,245
	mov BR18.yaxis,65
	
	mov ballY, 163
	mov ballX, 158
	mov ballLeft, 1
	mov ballUp, 1
		
	mov strikerX, 140
	mov strikerY, 170
	mov stlength,40
	mov speed,2
	mov scoreCount,0
	mov lvscore,18
	mov chec,0
	mov levchec,0
	mov fixy,0

	ret
Level1_Normalize endp

Level2_Normalize proc uses ax bx


	mov ax,scoreCount
	mov bx,10
	div bl
	add al,48
	add ah,48
	mov str1[25],31h
	mov str1[32],al
	mov str1[33],ah
	mov str1[39],10
	

	mov strlen,40
	call openfile
	call writefile
	call closefile

	mov BR1.open,1
	mov BR1.xaxis,45
	mov BR1.yaxis,25
	mov BR2.open,1
	mov BR2.xaxis,85
	mov BR2.yaxis,25
	mov BR3.open,1
	mov BR3.xaxis,205
	mov BR3.yaxis,25
	mov BR4.open,1
	mov BR4.xaxis,245
	mov BR4.yaxis,25
	mov BR5.open,1
	mov BR5.xaxis,45
	mov BR5.yaxis,45
	mov BR6.open,1
	mov BR6.xaxis,85
	mov BR6.yaxis,45
	mov BR7.open,1
	mov BR7.xaxis,205
	mov BR7.yaxis,45
	mov BR8.open,1
	mov BR8.xaxis,245
	mov BR8.yaxis,45
	mov BR9.open,1
	mov BR9.xaxis,45
	mov BR9.yaxis,65
	mov BR10.open,1
	mov BR10.xaxis,85
	mov BR10.yaxis,65
	mov BR11.open,1
	mov BR11.xaxis,205
	mov BR11.yaxis,65
	mov BR12.open,1
	mov BR12.xaxis,245
	mov BR12.yaxis,65
	mov BR13.open,1
	mov BR13.xaxis,45
	mov BR13.yaxis,85
	mov BR14.open,1
	mov BR14.xaxis,85
	mov BR14.yaxis,85
	mov BR15.open,1
	mov BR15.xaxis,205
	mov BR15.yaxis,85
	mov BR16.open,1
	mov BR16.xaxis,245
	mov BR16.yaxis,85
	mov BR17.open,2
	mov BR17.xaxis,125
	mov BR17.yaxis,55
	mov BR18.open,2
	mov BR18.xaxis,165
	mov BR18.yaxis,55
	
	mov ballY, 163
	mov ballX, 158
	mov ballLeft, 1
	mov ballUp, 1
		
	mov strikerX, 140
	mov strikerY, 170
	mov stlength,35
	mov speed,3
	mov scoreCount,0
	mov lvscore,20
	mov chec,0
	mov levchec,1

	ret
Level2_Normalize endp

Level3_Normalize proc

	mov ax,scoreCount
	mov bx,10
	div bl
	add al,48
	add ah,48
	mov str1[25],32h
	mov str1[32],al
	mov str1[33],ah
	mov str1[39],10
	
	
	mov strlen,40
	call openfile
	call writefile
	call closefile

	mov BR1.open,1
	mov BR1.xaxis,45
	mov BR1.yaxis,25
	mov BR2.open,1
	mov BR2.xaxis,85
	mov BR2.yaxis,25
	mov BR3.open,1
	mov BR3.xaxis,205
	mov BR3.yaxis,25
	mov BR4.open,1
	mov BR4.xaxis,245
	mov BR4.yaxis,25
	mov BR5.open,1
	mov BR5.xaxis,45
	mov BR5.yaxis,45
	mov BR6.open,1
	mov BR6.xaxis,85
	mov BR6.yaxis,45
	mov BR7.open,1
	mov BR7.xaxis,205
	mov BR7.yaxis,45
	mov BR8.open,1
	mov BR8.xaxis,245
	mov BR8.yaxis,45
	mov BR9.open,1
	mov BR9.xaxis,45
	mov BR9.yaxis,65
	mov BR10.open,1
	mov BR10.xaxis,85
	mov BR10.yaxis,65
	mov BR11.open,1
	mov BR11.xaxis,205
	mov BR11.yaxis,65
	mov BR12.open,1
	mov BR12.xaxis,245
	mov BR12.yaxis,65
	mov BR13.open,1
	mov BR13.xaxis,45
	mov BR13.yaxis,85
	mov BR14.open,1
	mov BR14.xaxis,85
	mov BR14.yaxis,85
	mov BR15.open,1
	mov BR15.xaxis,205
	mov BR15.yaxis,85
	mov BR16.open,1
	mov BR16.xaxis,245
	mov BR16.yaxis,85
	mov BR17.open,3
	mov BR17.xaxis,125
	mov BR17.yaxis,55
	mov BR18.open,3
	mov BR18.xaxis,165
	mov BR18.yaxis,55
	
	mov ballY, 163
	mov ballX, 158
	mov ballLeft, 1
	mov ballUp, 1
		
	mov strikerX, 140
	mov strikerY, 170
	mov stlength,35
	mov speed,3
	mov scoreCount,0
	mov lvscore,22
	mov chec,0
	mov levchec,2
	mov fixy,0

	ret
Level3_Normalize endp

;-------------------------------------------------------------------------------

Level2Menu proc

			call Clear_Screen
			mov dh, 4 ;row
			mov dl, 10 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset lvv2
			mov ah, 9
			int 21h
	
			mov dh, 8 ;row
			mov dl, 16 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset lv2
			mov ah, 9
			int 21h
			
			mov dh, 12 ;row
			mov dl, 6 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset inst4
			mov ah, 9
			int 21h
			
			llvl2:
			mov ah,1
			int 16h
			jz llvl2
			call Clear_Screen
	jmp Level2Main
  ret
Level2Menu endp

Level3Menu proc


			call Clear_Screen

			mov dh, 4 ;row
			mov dl, 10 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset lvv3
			mov ah, 9
			int 21h
	
			mov dh, 8 ;row
			mov dl, 16 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset lv3
			mov ah, 9
			int 21h
			
			mov dh, 12 ;row
			mov dl, 6 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset inst4
			mov ah, 9
			int 21h
			
			llvl3:
			mov ah,1
			int 16h
			jz llvl3
			
			call Clear_Screen
			jmp Level3Main
			
			
	
  ret
Level3Menu endp


WinMenu proc


			call Clear_Screen

			mov dh, 6 ;row
			mov dl, 13 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset Gam
			mov ah, 9
			int 21h
			
			mov dh, 12 ;row
			mov dl, 6 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset inst4
			mov ah, 9
			int 21h
			
			mov ah,1
			int 21h
			
			call Clear_Screen
	
  ret
WinMenu endp

PauseMenu proc

			call Clear_Screen

			mov dh, 5 ;row
			mov dl, 14 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset pau
			mov ah, 9
			int 21h
	
			mov dh, 8 ;row
			mov dl, 17 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset pau1
			mov ah, 9
			int 21h
			
			mov dh, 12 ;row
			mov dl, 6 ;col
			mov ah, 2 
			int 10h
			
			mov dx, offset inst4
			mov ah, 9
			int 21h
			
			mov ah,1
			int 21h
			cmp al,'1'
			je MAIN_Start
			call Clear_Screen
  ret
PauseMenu endp


;----------------------------------File Handling---------------------------------

openfile proc

	mov ah,3dh 
	mov al,2 
	mov dx,offset fname
	int 21h 

	mov handle,ax
	ret

openfile endp

writefile proc

	mov cx,0
	mov dx, 0
	mov bx,handle
	mov ah,42h
	mov al,2
	int 21h
	mov ah, 40h
	mov bx, handle
	mov cx, strlen

	mov dx, offset str1
	int 21h

	ret
	
writefile endp

closefile proc
	mov ah, 3eh
	mov bx, handle
	int 21h
	ret
closefile endp


;--------------------------------------------------------------------------------

InputName proc uses si ax bx cx
	
	mov cx,20
	mov si,0
	mov ax,0
	mov bx,0
	
	mov dh, 12 ;row
	mov dl, 6 ;col
	mov ah, 2 
	int 10h
			
	mov dx, offset strname
	mov ah, 9
	int 21h
	
	mov dh, 13 ;row
	mov dl, 6 ;col
	mov ah, 2 
	int 10h
	
	inpu:
		mov ah,1
		int 21h
		cmp al,13
		je inpuend
		mov str1[si],al
		inc si
	loop inpu

	inpuend:
	
	call Clear_Screen
	
	ret
InputName endp

main:
	mov ax,@data
	mov ds,ax
	mov ax,0
	mov bx,0
	
		
		mov color,4
		; Setting Graphics Mode
			mov ah, 0
			mov al, 13h 
			int 10h    
		
		; Drawing Boundary
			;------TOP------------
			mov b_x,20
			mov e_x,300
			mov b_y,5
			mov e_y,8
			call draw
			;------RIGHT------------
			mov b_x,297
			mov e_x,300
			mov b_y,7
			mov e_y,180
			call draw
			;------LEFT------------
			mov b_x,20
			mov e_x,23
			mov b_y,7
			mov e_y,180
			call draw
			;------BOTTOM------------
			mov b_x,20
			mov e_x,300
			mov b_y,177
			mov e_y,180
			call draw 
			
		; Main Menu
			MAIN_Start:
			call Clear_Screen
			call Level1_Normalize
			call MAINMENU
			call Clear_Screen
			call InputName
		; Creating First Level
			call Level1Menu
			call Clear_Screen
			;Setting Color
			mov color,7
			
			mov ax, BR1.xaxis
			mov bx, BR1.yaxis
			call AddBrick
			
			mov ax, BR2.xaxis
			mov bx, BR2.yaxis
			call AddBrick
			
			mov ax, BR3.xaxis
			mov bx, BR3.yaxis
			call AddBrick
			
			mov ax, BR4.xaxis
			mov bx, BR4.yaxis
			call AddBrick
			
			mov ax, BR5.xaxis
			mov bx, BR5.yaxis
			call AddBrick
			
			mov ax, BR6.xaxis
			mov bx, BR6.yaxis
			call AddBrick
			
			mov ax, BR7.xaxis
			mov bx, BR7.yaxis
			call AddBrick
			
			mov ax, BR8.xaxis
			mov bx, BR8.yaxis
			call AddBrick
			
			mov ax, BR9.xaxis
			mov bx, BR9.yaxis
			call AddBrick
			
			mov ax, BR10.xaxis
			mov bx, BR10.yaxis
			call AddBrick
			
			mov ax, BR11.xaxis
			mov bx, BR11.yaxis
			call AddBrick
			
			mov ax, BR12.xaxis
			mov bx, BR12.yaxis
			call AddBrick
			
			mov ax, BR13.xaxis
			mov bx, BR13.yaxis
			call AddBrick
			
			mov ax, BR14.xaxis
			mov bx, BR14.yaxis
			call AddBrick
			
			mov ax, BR15.xaxis
			mov bx, BR15.yaxis
			call AddBrick
			
			mov ax, BR16.xaxis
			mov bx, BR16.yaxis
			call AddBrick
			
			mov ax, BR17.xaxis
			mov bx, BR17.yaxis
			call AddBrick
			
			mov ax, BR18.xaxis
			mov bx, BR18.yaxis
			call AddBrick
			
		; Draw Striker
			mov color, 7
			call drawStriker
		
		; Draw Ball
			mov color, 3
			call drawball
			
		; Live Scores
			call DrawLivesScores
			
		; GameLoop
			call gameLoop  
	
	Level2Main:
		call Level2_Normalize
		mov color,7
			
			mov ax, BR1.xaxis
			mov bx, BR1.yaxis
			call AddBrick
			
			mov ax, BR2.xaxis
			mov bx, BR2.yaxis
			call AddBrick
			
			mov ax, BR3.xaxis
			mov bx, BR3.yaxis
			call AddBrick
			
			mov ax, BR4.xaxis
			mov bx, BR4.yaxis
			call AddBrick
			
			mov ax, BR5.xaxis
			mov bx, BR5.yaxis
			call AddBrick
			
			mov ax, BR6.xaxis
			mov bx, BR6.yaxis
			call AddBrick
			
			mov ax, BR7.xaxis
			mov bx, BR7.yaxis
			call AddBrick
			
			mov ax, BR8.xaxis
			mov bx, BR8.yaxis
			call AddBrick
			
			mov ax, BR9.xaxis
			mov bx, BR9.yaxis
			call AddBrick
			
			mov ax, BR10.xaxis
			mov bx, BR10.yaxis
			call AddBrick
			
			mov ax, BR11.xaxis
			mov bx, BR11.yaxis
			call AddBrick
			
			mov ax, BR12.xaxis
			mov bx, BR12.yaxis
			call AddBrick
			
			mov ax, BR13.xaxis
			mov bx, BR13.yaxis
			call AddBrick
			
			mov ax, BR14.xaxis
			mov bx, BR14.yaxis
			call AddBrick
			
			mov ax, BR15.xaxis
			mov bx, BR15.yaxis
			call AddBrick
			
			mov ax, BR16.xaxis
			mov bx, BR16.yaxis
			call AddBrick
			
			mov ax, BR17.xaxis
			mov bx, BR17.yaxis
			call AddBrick
			
			mov ax, BR18.xaxis
			mov bx, BR18.yaxis
			call AddBrick
			
			; Draw Striker
			mov color, 7
			call drawStriker
		
		; Draw Ball
			mov color, 3
			call drawball
			
		; Live Scores
			call DrawLivesScores
			
		; GameLoop
			call gameLoop 
			
			Level3Main:
			call Level3_Normalize
			mov color,7
			
			mov ax, BR1.xaxis
			mov bx, BR1.yaxis
			call AddBrick
			
			mov ax, BR2.xaxis
			mov bx, BR2.yaxis
			call AddBrick
			
			mov ax, BR3.xaxis
			mov bx, BR3.yaxis
			call AddBrick
			
			mov ax, BR4.xaxis
			mov bx, BR4.yaxis
			call AddBrick
			
			mov ax, BR5.xaxis
			mov bx, BR5.yaxis
			call AddBrick
			
			mov ax, BR6.xaxis
			mov bx, BR6.yaxis
			call AddBrick
			
			mov ax, BR7.xaxis
			mov bx, BR7.yaxis
			call AddBrick
			
			mov ax, BR8.xaxis
			mov bx, BR8.yaxis
			call AddBrick
			
			mov ax, BR9.xaxis
			mov bx, BR9.yaxis
			call AddBrick
			
			mov ax, BR10.xaxis
			mov bx, BR10.yaxis
			call AddBrick
			
			mov ax, BR11.xaxis
			mov bx, BR11.yaxis
			call AddBrick
			
			mov ax, BR12.xaxis
			mov bx, BR12.yaxis
			call AddBrick
			
			mov ax, BR13.xaxis
			mov bx, BR13.yaxis
			call AddBrick
			
			mov ax, BR14.xaxis
			mov bx, BR14.yaxis
			call AddBrick
			
			mov ax, BR15.xaxis
			mov bx, BR15.yaxis
			call AddBrick
			
			mov ax, BR16.xaxis
			mov bx, BR16.yaxis
			call AddBrick
			
			mov ax, BR17.xaxis
			mov bx, BR17.yaxis
			call AddBrick
			
			mov ax, BR18.xaxis
			mov bx, BR18.yaxis
			call AddBrick
			
			; Draw Striker
			mov color, 7
			call drawStriker
		
		; Draw Ball
			mov color, 3
			call drawball
			
		; Live Scores
			call DrawLivesScores
			
		; GameLoop
			call gameLoop 
			
		endGame:
			
			mov ax,scoreCount
			mov bx,10
			div bl
			add al,48
			add ah,48
			mov str1[25],32h
			mov str1[32],al
			mov str1[33],ah
			mov str1[39],10
			mov strlen,40
			call openfile
			call writefile
			call closefile
			call WinMenu
			jmp MAIN_Start
	
mov ah,4ch
int 21h
end
.model samll
.stack 300h
.data 
;-----------------------------------------------------------------------------------------
stdid db 0dh,0ah,9,9,"Enter Student ID as '1 : $"
stdrollno db 0dh,0ah,9,9,"Enter Student Roll No as 'a1 : $"
stdname db 0dh,0ah,9,9,"Enter Student Name as 'ahmed : $"
class   db 0dh,0ah,9,9,"Enter Class as'9th  :$"
subject db 0dh,0ah,9,9,"Enter Subject as' chem-maths :$"
session db 0dh,0ah,9,9,"Enter Session as' fall-2016 :$"
teacher db 0dh,0ah,9,9,"Enter teacher Name as' sir ali :$"
dues db 0dh,0ah,9,9,"Enter dues as' 2000rs :$"
successupdate db 0dh,0ah,9,9,"Record updated successfully hit any key to go back....$"
insertsuccess db 0dh,0ah,9,9,"Record inserted successfully....$"
stoppingby  db 0dh,0ah,0dh,0ah,0dh,0ah
            db 0dh,0ah,9,9," ---------------------------------------------"
            db 0dh,0ah,9,9,"|                                             |"
            db 0dh,0ah,9,9,"|        Thanks for Stopping by........!      |"
            db 0dh,0ah,9,9,"|        We Hope to see you again......!      |"
			db 0dh,0ah,9,9,"|                                             |"
			db 0dh,0ah,9,9," --------------------------------------------- $"
            
;_____________________________________________________________________________________________
keytosearch db 0dh,0ah,9,9,"Enter RollNo of student : $"
keynotfound db 0dh,0ah,9,9,"student not found try again....$"
keyfound db 0dh,0ah,9,9,"student found press any key to view....$"
updateprompt db 0dh,0ah,9,9,"press'0'to cancel update or press any key to continue...$"
removeprompt db 0dh,0ah,9,"Record will be deleated press'0'to cancel or any key to continue.....$"
successremove db 0dh,0ah,9,9,"Records deleted successfully hit any key to go back....$"
;_____________________________________________________________________________________________
$msg db "$"
prskeytoexit db 0dh,0ah,9,9,"Press any key to exit ....................$"
prskey db 0dh,0ah,9,9,"Press any key ....................$"
resetprompt db 0dh,0ah,9,9,"All record will be deleated..........."
            db 0dh,0ah,9,9,"press '0' to cancel or any key to continue :$"
systemclered db 0dh,0ah,9,9,"All system cleared successfully hit any key to login again.......$"
;________________________LOGIN PAGE____________________________________
loginpage db 0dh,0ah,9,9," _____________________________________________"
         db 0dh,0ah,9,9,"|  WELCOME TO SMS(School Management System)   |"
         db 0dh,0ah,9,9,"|---------------------------------------------|"
         db 0dh,0ah,9,9,"|                                             |"
         db 0dh,0ah,9,9,"|  Press 1  --> To LogIn As ADMIN             |"
         db 0dh,0ah,9,9,"|  Press 2  --> Credit                        |"
         db 0dh,0ah,9,9,"|  Press 3  --> Help                          |"
         db 0dh,0ah,9,9,"|  Press 4  --> To EXIT                       |"
         db 0dh,0ah,9,9,"|                                             |"
         db 0dh,0ah,9,9,"|_____________________________________________|$"
loginprompt db 0dh,0ah,9,9," Choose (1 to 4) --> $"	
loginerror db 0dh,0ah 
         db 0dh,0ah,9,9," Please Select Just (1 to 4) --> $"	
username db 0dh,0ah,9,9," Enter Username --->$"
password db 0dh,0ah,9,9," Enter Password --->$" 		 
uname db 50   ; max length 
      db ?   ; enter chracters
      db 50 dup(0) ;stored uname
pwd   db 50   ; max length 
      db ?   ; enter chracters
      db 50 dup(0) ;stored uname
notfound db 0dh,0ah,9,9," username or password does not matching!$"
found db 0dh,0ah,9,9,"login successfully !$"

;_______________________________________________________________________

;________________________MAINMENUE______________________________________
welcome  db 0dh,0ah,9,9," _____________________________________________"
         db 0dh,0ah,9,9,"|          WELCOME ADMIN -->$"
startscreen db 0dh,0ah,9,9,"|---------------------------------------------|"
         db 0dh,0ah,9,9,"|                                             |"
         db 0dh,0ah,9,9,"|  Press 1  --> To Add New Student            |"
         db 0dh,0ah,9,9,"|  Press 2  --> To View Students Records      |"
         db 0dh,0ah,9,9,"|  Press 3  --> To Update Students Records    |"
         db 0dh,0ah,9,9,"|  Press 4  --> To Remove Student From Records|"
         db 0dh,0ah,9,9,"|  Press 5  --> To Search Records             |"
         db 0dh,0ah,9,9,"|  Press 6  --> To Reset System               |"
         db 0dh,0ah,9,9,"|  Press 7  --> To Logout                     |"
         db 0dh,0ah,9,9,"|                                             |"
         db 0dh,0ah,9,9,"|_____________________________________________|$"
startscreenprompt db 0dh,0ah 
         db 0dh,0ah,9,9," Choose (1 or 7) --> $"		 
startscreenerr db 0dh,0ah 
         db 0dh,0ah,9,9," Please Select just (1 or 7)--> $"		 		
viewscreen db 0dh,0ah,20h,20h," ___________________________________________________________________"
           db 0dh,0ah,20h,20h,"|(ID)(RollNo)(Name)(Class)(Subjects) (Session)   (Teacher)   (dues) |"
		   db 0dh,0ah,20h,20h," ------------------------------------------------------------------- $"
nextline db 0dh,0ah,20h,20h,"$"		   
;________________________________________________________________________


;________________________ ABOUT US ______________________________________
about    db 0dh,0ah,9,9," _____________________________________________"
         db 0dh,0ah,9,9,"|   This SMS (School ManagemenSystem)         |"
         db 0dh,0ah,9,9,"|   is Team Project Of COAL Created By        |"
         db 0dh,0ah,9,9,"|---------------------------------------------|"
		 db 0dh,0ah,9,9,"|                                             |"
         db 0dh,0ah,9,9,"|    *  Muhammad Hamza   BITF14M 513          |"
         db 0dh,0ah,9,9,"|    *  Muhammad Ahmed   BITF14M 553          |"
         db 0dh,0ah,9,9,"|    *  Bilal Khan       BITF14M 505          |"
         db 0dh,0ah,9,9,"|                                             |"
         db 0dh,0ah,9,9,"|_____________________________________________|$"
;_______________________________________________________________________
		 

;________________________Help___________________________________________
help     db 0dh,0ah,9,9," ______________________________________________________"
         db 0dh,0ah,9,9,"|           HELP TO SMS(School Management System)      |"
         db 0dh,0ah,9,9,"|______________________________________________________|"
         db 0dh,0ah,9,9,"|                                                      |"
         db 0dh,0ah,9,9,"|   a: This Program is puerly designed for masm 0.74   |"
		 db 0dh,0ah,9,9,"|   b: It will not work on emulater                    |"
         db 0dh,0ah,9,9,"|   c: Please login to access system main screen       |"
         db 0dh,0ah,9,9,"|   d: you can not use system without admin privilages |"
         db 0dh,0ah,9,9,"|   e: Choose Valid Option Each time to work Correctly |"
         db 0dh,0ah,9,9,"|   f: Otherwise System Will Terminate itself.         |"
         db 0dh,0ah,9,9,"|                                                      |"
         db 0dh,0ah,9,9,"|______________________________________________________|$"
;_______________________________________________________________________


;________________________Thanks_________________________________________
thank    db 0dh,0ah,9,9," _________________________________________________"
         db 0dh,0ah,9,9,"|Thanks To Use Our SMS(School Management System)  |"
         db 0dh,0ah,9,9,"|-------------------------------------------------|"
         db 0dh,0ah,9,9,"|     Shutting Down System ...................    |"
         db 0dh,0ah,9,9,"|_________________________________________________|$"
;_______________________________________________________________________

;________________________File data______________________________________________
fileerr db 0dh,0ah,9,9,"file error code is$:-->"
handel dw ?
key db 10
    db ?
	db 10 dup(0)
account db "account.txt",0
student db "student.txt",0
accountarray db 30 dup(0)
studentarray db 1000 dup(0),'$'
testarray db 50 dup(0)
stdupdater db 100 dup(0)
stdtemp db 100 dup(0)
;_______________________________________________________________________________		 
.code
main proc
        mov ax,@data 
        mov ds,ax   
        call login
        mov ah,4ch
        int 21h       
main endp
;____________________________________________________________________add admin_________________________-
addadmin proc
       ;postponed......
addadmin endp
;________________________login function_______________________________________________________
login proc
 printagain:
    call setscreen
    mov cl,3
    lea dx,loginpage
    mov ah,9h
    int 21h	
 	lea dx,loginprompt
	mov ah,9h
	int 21h
    loginmain: 
        mov ah,1h
        int 21h
		cmp al,'1'
	    je callogincheck
	    cmp al,'2'
		je callcredit
		cmp al,'3'
		je callhelp
		cmp al,'4'
		je callthankyou
	    jmp loginerr
    callogincheck:    		
        call logincheck          ; calling loginchecks
		cmp bl,1
		je printagain
     	ret
    callcredit:    		
        call credit          
        jmp printagain
	callthankyou:    		
        call thankyou          ; calling thank
        ret
	callhelp:    		
        call helpus          ; calling help
        jmp printagain	
    loginerr:
	    dec cl
		jz goreturn
	    lea dx,loginerror
		mov ah,9h
		int 21h
		jmp loginmain
	goreturn:
        ret	
login endp
logincheck proc    
    ; call to menue if login valid
    ; else invalid id ask for 3 times if invalid then exit system
    push cx	
    mov cl,3
    authentication:
        lea dx,username
        mov ah,9h
        int 21h
        mov ah,0ah
        mov dx,offset uname
        int 21h
		mov si,offset uname+1
		call insert         ; change 0dh with $
		lea dx,password     ; asking password
        mov ah,9h
        int 21h 
        
		;;;; work for hide chracter
		
		mov dx,offset pwd 
        mov ah,0ah
        int 21h
		mov si,offset pwd+1
		call insert         ; change 0dh with $ 		   
        call validation   
		;call mainmenue
		cmp al,1
		je loginsuccess        ; use flag if username and password is valid and call to main menue
        cmp al,2
		je logincheckerr
		ret
	logincheckerr:
        lea dx,notfound
        mov ah,9h
        int 21h
        dec cl
        jnz authentication
        pop cx		
        ret
    loginsuccess:
        lea dx,found
        mov ah,9h
        int 21h
		lea dx,prskey
		int 21h
		mov ah,1h
		int 21
		call mainmenue
		pop cx		
        ret
logincheck endp
insert proc          ; change 0dh with 13 in input string
    push cx
    mov cl,[si]      ; saving number of input chracters 
	mov ch,0
	inc cx           ; reached at position where 0dh saved
	add si,cx
	mov al,'$'
	mov [si],al
	pop cx
	ret
insert endp 
validation proc   
    push cx               ; validation from file account.txt
	lea dx,account        ; file name to open
	call openexistingfile ; calling to openexistingfile
	lea dx,accountarray   ; array where accounts file data has to save
	call readfile          
	call closefile
	mov dx,[offset uname+1] ; length of username
	mov si,offset uname+2   ; input username from user
	mov di,offset testarray
	mov cl,2
	makingarray:
        mov al,[si]
        mov [di],al
		cmp al,'$'
        je increment
        inc di
		inc si
		jmp makingarray
    increment:
	    mov al,';'
		mov [di],al
		cmp cl,1
		je adde
	    dec cl
		mov si,offset pwd+2
		inc di
		jnz makingarray
    adde:
	    mov al,';'
		mov [di],al
		inc di
		mov al,0dh
		mov [di],al
		inc di
		mov al,'$'
		mov [di],al
	checking:
	    mov si,offset accountarray        ;username and password from file 
        mov di,offset testarray
;________
    mov cl,30h
    comparing:
        mov dl,[si]
		mov dh,[di]
		cmp dl,dh
		je checkbothfor0dh
		cmp dl,0dh
		je resettestarray
		cmp dh,0dh
		je reachenter		
	reachenter:
	    inc si
		mov al,[si]
		cmp al,'$'
		je return_false
		cmp al,0dh
		je resettestarray 
		jmp reachenter			
	resettestarray:
	    inc si
	    inc si
		mov dl,[si]
        mov di,offset testarray
        jmp comparing		
	checkbothfor0dh:
	    cmp dl,0dh
		je return_true
		inc si
		inc di
		jmp comparing
    return_true:
       	pop cx
		mov al,1    ; making  flag set if found
		ret	
    return_false:
	    pop cx
		mov al,2    ; making  flag reset 
		ret
validation endp
;_______________________________________________main menue___________________________________
mainmenue proc
 menueagain:  
    call setscreen
	lea dx,welcome
	mov ah,9h
	int 21h
	mov dx,offset uname+2     ; giving name to admin which is login
	mov ah,9h
    int 21h
    lea dx,startscreen
    mov ah,9h
    int 21h 
    lea dx,startscreenprompt
    mov ah,9h 
    int 21h
    menue:
        mov ah,1h
        int 21h
        cmp al,'1'
        je addstudent
        cmp al,'2'
		je view
		cmp al,'3'
		je update
        cmp al,'4'
		je remove
		cmp al,'5'
		je search
        cmp al,'6'
		je reset
		cmp al,'7'
		je logout       		
        jmp menueerr
    addstudent:
	    call addfunc
	    jmp menueagain
	view:
		call viewrecords
		jmp menueagain
	update:
	    call updatefunc
	    jmp menueagain
	tempmenueagain:      ; mile stone 128byte
        jmp menueagain	
	remove:
	    call removefunc
	    jmp menueagain
	search:
	    call searchfunc
	    jmp menueagain
	reset:
	    confirm:
	        lea dx,resetprompt     ; confirmation to delete
		    mov ah,9h
		    int 21h
		    mov ah,1h
		    int 21h
		    cmp al,'0'
		    je tempmenueagain
        goreset:			
		    lea dx,student
		    call opennewfile
		    call closefile
			lea dx,student
			call openexistingfile   ; in read write mode  to save $ in file
			lea dx,$msg
			mov cx,1
			call writefile
			call closefile
		    lea dx,systemclered
		    mov ah,9h
		    int 21h
		    mov ah,1h
	     	int 21h		
			jmp logout
	jmp menueagain
	logout:
	    call setscreen
		lea dx,stoppingby
		mov ah,9h
		int 21h
		lea dx,prskey
		mov ah,9h
		int 21h
		mov ah,1h
		int 21h
	    mov si,offset testarray    ; making temperary array null
;---------------------------------------------------------
	    mov cl,50
		t:
		    mov dl,0
		    mov [si],dl
			inc si
			dec cl
			jnz t
		mov si,offset uname+1
        mov cl,[si]
        u:
		    inc si
			mov dl,0
            mov [si],dl
            dec cl
            jnz u
		mov si,offset pwd+1
        mov cl,[si]
        p:
		    inc si
			mov dl,0
            mov [si],dl
            dec cl
            jnz p     		
;---------------------------------------------------------		 
	    mov bl,1            ; check to go to login again menue
        ret
    menueerr:
        lea dx,startscreenerr
        mov ah,9h
        int 21h
        jmp menue    
mainmenue endp
place$file proc
    push cx
	push dx
	lea dx,student
	call opennewfile
	lea dx,$msg
	mov cx,1
	call writefile
	call closefile
 	pop dx
	pop cx
	ret
place$file endp
;--------------------------------------------------------------------------------------------
addfunc proc
   processagain:   
        call setscreen
        mov bl,'a'  ;flag that say called from add function
	    call newvalues
	    lea dx,student
	    call openexistingfile
		call nullarrays
		lea dx,studentarray
	    call readfile
	    call closefile
		mov si,offset studentarray
	    mov bl,0
	    cmp bl,[si]
	    je place$infile 
		mov cl,9
	eof:
        mov dl,[si]
        inc si
        cmp dl,'$'
		jne eof
		dec si  ;pointing $ sign at endofile
        ;getting last id  to auto increment
		;mov di,si  ; saving eof pointer
		mov di,offset stdupdater
		concatenate:
		    mov dl,[di]
			mov [si],dl
			inc di
			inc si
			cmp dl,'$'
			jne concatenate
			lea dx,student
			call opennewfile
			call lengthofrecord  ; no of bytes of record
			lea dx,studentarray
			call writefile
			call closefile
			lea dx,insertsuccess
			mov ah,9h
			int 21h
			lea dx,prskey
			mov ah,9h
			int 21h
			mov ah,1h
			int 21h
			call viewrecords
			jmp returning
	place$infile:
	    call place$file
		jmp processagain
;--------------------------------------------------------auto increment in id get closed-----------------
    ;    getid:
	;	    mov dl,[si]
	;	    cmp dl,';'
	;		je countcln
	;	    dec si
	;		jmp getid
	;	countcln:
    ;       dec si
    ;        dec cl			
	;	    jnz getid    ;incrementing to get si of last id
	;		inc si
	;		inc si
	;		inc si
	;		inc si
	;		mov dl,[si]
    ;       mov ah,2
    ;        int 21h
    ;        mov ah,4ch
    ;        int 21h		
	;	getsiofid:          ;get the starting offset of id at eof
    ;        mov dl,[si]
	;		cmp dl,';'
	;		dec si
    ;        jne getsiofid
    ;        inc si
	;		inc si
;--------------------------------------------------------auto increment in id get closed-----------------  
	returning:
	    ret    
addfunc endp
;--------------------------------------------------------------------------------------------
updatefunc proc
    call searchfunc
	cmp bl,0 
	je cancelupdate
	lea dx,updateprompt
	mov ah,9h
	int 21h
	mov ah,1h
	int 21h
	cmp al,'0'
	je cancelupdate
	call newvaluesforupdate    ;------------------------
	mov ch,0
    mov bx,si   ;svaing next record index 
	mov di,offset stdtemp
	copsying:  
	    mov dl,[si]
		mov [di],dl
		inc si
		inc di
		cmp dl,'$'
		jne copsying		
	mov si,bx
	sub si,cx               ;index at record to update 
    mov bx,cx               ;number of bytes under consideration in bx
	mov cl,2
 	skipfirsttwocolon:
	    mov bl,[si]
		inc si
		cmp bl,';'
		jne skipfirsttwocolon
		dec cl
		jnz skipfirsttwocolon
	mov di,offset stdupdater   ; assigning which array to update
	updateing:
	    mov dl,[di]
        mov [si],dl
        inc si
        inc di		
	    cmp dl,'$'
		jne updateing
        dec si   ;wher $ present in stdupdater
		;saving record after the updated record
    mov di,offset stdtemp
    savingpost:
        mov dl,[di]
        mov [si],dl
		inc si 
		inc di		
		cmp dl,'$'
		jne savingpost 
    lea dx,student
	call opennewfile
	call lengthofrecord
	lea dx,studentarray
	call writefile
	call closefile
	lea dx,successupdate
    mov ah,9h
    int 21h 
    mov ah,1h
    int 21h	
	cancelupdate:
	    ret
updatefunc endp
savepost proc
    push di
	push si
	push cx
	mov di,offset stdtemp
	copying:  
	    mov cl,[si]
		mov [di],cl
		inc si
		inc di
		mov dl,cl
		cmp cl,'$'
		jne copying
	pop cx
	pop si
	pop di
	ret
savepost endp
newvalues proc
    push di
	push si
    push cx
	mov si,offset stdupdater
	cmp bl,'a'
	jne notaddfunc
	lea dx,stdid
	mov ah,9h 
	int 21h
    loopid:               ;asking new name
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne loopid
        dec si
        mov al,';'
        mov [si],al
        inc si 
    lea dx,stdrollno
	mov ah,9h 
	int 21h
    looprollno:               ;asking new name
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne looprollno
        dec si
        mov al,';'
        mov [si],al
        inc si		
notaddfunc:	
	lea dx,stdname
	mov ah,9h 
	int 21h
    loop1:               ;asking new name
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne loop1
        dec si
        mov al,';'
        mov [si],al
        inc si      
	lea dx,class        ;asking new class 
    mov ah,9h 
    int 21h         
	loop2:
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne loop2
        dec si
        mov al,';'
        mov [si],al
        inc si
	lea dx,subject
	mov ah,9h 
	int 21h 
	loop3:             ;asking new subject
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne loop3
        dec si
        mov al,';'
        mov [si],al
        inc si	
    lea dx,session
	mov ah,9h 
	int 21h                ;asking new session
	loop4:
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne loop4
        dec si
        mov al,';'
        mov [si],al
        inc si	
	lea dx,teacher          ;asking new teacher
	mov ah,9h 
    int 21h 
	loop5:
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne loop5
        dec si
        mov al,';'
        mov [si],al
        inc si
	lea dx,dues         ;asking new dueses
	mov ah,9h 
	int 21h 
	loop6:
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne loop6
        dec si
        mov al,';'
		mov [si],al
		inc si
		mov dl,0dh
		mov [si],dl
		mov dl,0ah
		inc si
		mov [si],dl
	mov al,'$'        ;placing $at the end of array
    inc si
	mov [si],al
	pop cx
	pop si
    pop di
	ret
newvalues endp
newvaluesforupdate proc
    push di
	push si
    push cx
	mov si,offset stdupdater
	lea dx,stdname
	mov ah,9h 
	int 21h
    loopu1:               ;asking new name
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne loopu1
        dec si
        mov al,';'
        mov [si],al
        inc si      
	lea dx,class        ;asking new class 
    mov ah,9h 
    int 21h         
	loopu2:
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne loopu2
        dec si
        mov al,';'
        mov [si],al
        inc si
	lea dx,subject
	mov ah,9h 
	int 21h 
	loopu3:             ;asking new subject
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne loopu3
        dec si
        mov al,';'
        mov [si],al
        inc si	
    lea dx,session
	mov ah,9h 
	int 21h                ;asking new session
	loopu4:
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne loopu4
        dec si
        mov al,';'
        mov [si],al
        inc si	
	lea dx,teacher          ;asking new teacher
	mov ah,9h 
    int 21h 
	loopu5:
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne loopu5
        dec si
        mov al,';'
        mov [si],al
        inc si
	lea dx,dues         ;asking new dueses
	mov ah,9h 
	int 21h 
	loopu6:
	    mov ah,1h
		int 21h
		mov [si],al
        inc si 
        cmp al,0dh
        jne loopu6
        dec si
		mov dl,[si]
        mov al,';'
        mov [si],al
        inc si
     ; end
	mov [si],dl
	mov al,'$'        ;placing $at the end of array
    inc si
	mov [si],al
	pop cx
	pop si
    pop di
	ret
newvaluesforupdate endp
;--------------------------------------------------------------------------------------------
removefunc proc
    call searchfunc
	cmp bl,0
    je cancelremove	
	lea dx,removeprompt
	mov ah,9h
	int 21h
	mov ah,1h
	int 21h
	cmp al,'0'
	je cancelremove
	                        ; cl contain count of how many time si incremented
	mov ch,0
    mov di,si               ; index at which next record present
	sub si,cx               ;index at record to delete 
    inc di
	transfer:
	    mov dl,[di]
		mov [si],dl
		inc si
		inc di
		cmp dl,'$'
		jne transfer        ; record deleted from logical array saving record to files	
	lea dx,student
	call opennewfile
	call lengthofrecord
	lea dx,studentarray
	call writefile
	call closefile
	lea dx,successremove
    mov ah,9h
    int 21h 
    mov ah,1h
    int 21h	
	cancelremove:
	    ret
removefunc endp
lengthofrecord proc
    push dx
	mov cx,0
	mov dl,0
	mov si,offset studentarray
	cmp cx,0
	je checkbyte
	countbytes:
	    inc cx
		mov dl,[si]
		inc si
		cmp dl,'$'
		jne countbytes
		jmp returned
	checkbyte:
	    cmp dl,[si]
		jne countbytes
        mov cx,1	
		mov dl,'$'
		mov [si],dl
    returned:		
		pop dx
	    ret
lengthofrecord endp
;--------------------------------------------------------------------------------------------
searchfunc proc
    lea dx,student
	call openexistingfile  ;in read write mode 
    call nullarrays
	lea dx,studentarray
	call readfile
    call closefile
    call askkeytouser
	mov di,offset key+2	
	mov si,offset studentarray
	finding:
	    mov cl,0 ; length of rollno
		mov ch,0  ;legth of id
      	skipfirstcolon:
            mov dl,[si]
			inc si
			inc ch
            cmp dl,';'
            jne skipfirstcolon
            searchcompare:
				mov dl,[si]	
			    mov bl,[di]
				cmp dl,bl
				je searchsemicolon
				mov cl,0
				mov ch,0
				jmp goenter
			searchsemicolon:
			    cmp dl,';'
				je searchtrue
				inc si
			    inc cl
				inc di
		    	jmp searchcompare
            goenter:
				inc si
			    mov dl,[si]
				cmp dl,'$'
				je searchfalse
            	cmp dl,0dh
                jne goenter
                inc si
                inc si
				mov di,offset key+2
                jmp skipfirstcolon
    searchtrue:
	    lea dx,keyfound
		mov ah,9h
		int 21h
		mov ah,1h
		int 21h
        call setscreen
	    ;dec ch    ; because count of rollno and id is one more than actual length
		lea dx,viewscreen
		mov ah,9h
		int 21h
        add cl,ch
		mov ch,0
        sub si,cx    ; now si pointing start of the search result	
	    lea dx,nextline
		mov ah,9h
		int 21h
        mov cl,0 		
		showsearched:
		    mov dl,[si]
			inc cl
            cmp dl,';'
			je colonskipper
			mov ah,2h
            int 21h
	    	inc si
			cmp dl,0dh
		    jne showsearched
			lea dx,prskey
			mov ah,9h
			int 21h
			mov ah,1h
			int 21h
    mov bl,1   ; flag found
    ret				
	colonskipper:
	    mov dl,20h
		int 21h
	    mov dl,20h
		mov ah,2h
		int 21h
	    mov dl,20h
		int 21h
		inc si
		jmp showsearched
    searchfalse:
	        lea dx,keynotfound
			mov ah,9h
			int 21h
		    lea dx,prskey
			mov ah,9h
			int 21h
			mov ah,1h
			int 21h
            mov bl,0   ;flag notfound			
			ret 
searchfunc endp
askkeytouser proc
    lea dx,keytosearch     ;prompt
	mov ah,9h
	int 21h
	mov ah,0ah
	lea dx,key    ;key to search
	int 21h	
	insert$_:
		mov si,offset key+1
        mov cl,[si]   ; length of input
		mov ch,0
		inc cx      ; at 2nd index 
		add si,cx   ;at last index
		mov bl,';'
		mov [si],bl
		inc si
		mov bl,'$'
		mov [si],bl	
		ret
askkeytouser endp
;--------------------------------------------------------------------------------------------
nullarrays proc
    push si
	push cx
	mov si,offset studentarray
    mov cx,1000
	nullify:
	    mov bl,0
	    mov [si],bl
		inc si
		dec cx
		jnz nullify
	pop cx
	pop si
	ret	
nullarrays endp
viewrecords proc
    call nullarrays
	lea dx,student
	call openexistingfile  ;in read write mode 
	lea dx,studentarray
	call readfile
	call closefile
    call setscreen
	lea dx,viewscreen
	mov ah,9h
	int 21h
	mov si,offset studentarray
	disp:
	    lea dx,nextline
		mov ah,9h
		int 21h
		content:
            mov dl,20h
			mov ah,2h
			int 21h
			mov dl,20h
			mov ah,2h
			int 21h
			continueprint:
			    mov dl,[si]
		        cmp dl,';'
				je remove_
				cmp dl,'$'
				je returnback
	            mov ah,2
	            int 21h
				inc si
				cmp dl,0dh
				je gonextline
				jmp continueprint
			gonextline:
                inc si
                jmp disp
            remove_:
                mov dl,20h
				mov ah,2h
				int 21h
                inc si
                jmp content    			
    returnback:
	    lea dx,prskey
		mov ah,9h
		int 21h
		mov ah,1
		int 21h
        ret	
viewrecords endp
;________________________File Handeling______________________________________________________________
opennewfile proc
    push cx
	push ax
	mov ah,3ch
	mov cl,1  ;read only attribute
	int 21h
	jc fileerror
	mov handel,ax
	pop ax
	pop cx
	ret
opennewfile endp
openexistingfile proc
    push ax
	mov ah,3dh
	mov al,2   ;access mode 
	int 21h
	jc fileerror
	mov handel,ax
	pop ax
	ret
openexistingfile endp
readfile proc
    push ax
	push bx
	push cx
	readagain: 
	    mov ah,3fh
	    mov bx,handel
	    mov cx,1024    ;1024 bytes data from file 
	    cmp cx,ax
		int 21h
	    ;jg readagain  ; there is more data in file so read again 
		jc fileerror
	pop cx
    pop bx
    pop ax	
    ret
readfile endp
writefile proc
        push bx
		push ax
		mov ah,40h
		mov bx,handel
		int 21h
		jc fileerror
		pop ax
		pop bx
		ret
writefile endp
closefile proc
        push bx
		push ax
        mov ah,3eh
		mov bx,handel  
		int 21h
		jc fileerror
		pop ax
		pop bx
		ret
    fileerror:
        lea dx,fileerr
		mov ah,9h
		int 21h
		add ax,30h
        mov dl,bl
        mov ah,2h
        int 21h
        mov ah,4ch
        int 21h		
closefile endp
;_____________________________________________________________________________________________
;________________________Screen color and positioning_________________________________________ 
setscreen proc
    push cx
	push ax 
	push bx
	push dx
    mov ax,0600h   ;06 is for row and 00 is for column
    mov bh,000eh   ; 6 for background 1 for text color
    mov cx,0000h
    mov DX,184Fh
    int 10h
    mov ah,02h
    mov bh,00
    mov dx,0319h    ;horizontal axis last two numbers  and first two numbers are for vertical positioning
    int 10h
	pop dx
	pop bx
	pop ax
	pop cx
	ret
setscreen endp
;________________________________________________________________________________________________
thankyou proc
    call setscreen
    lea dx,thank
    mov ah,9h
    int 21h
	lea dx,prskeytoexit
	mov ah,9h
	int 21h
	mov ah,1h
	int 21h
    ret 
thankyou endp
;________________________________________________________________________________________________
credit proc
    call setscreen
	lea dx,about
	mov ah,9h
	int 21h
	lea dx,prskey
	mov ah,9
	int 21h
	mov ah,1
	int 21h
    ret	
credit endp
;________________________________________________________________________________________________
helpus proc
   call setscreen
   lea dx,help
   mov ah,9h
   int 21h
   lea dx,prskey
   mov ah,9
   int 21h
   mov ah,1
   int 21h
   ret
helpus endp
;________________________________________________________________________________________________
end main

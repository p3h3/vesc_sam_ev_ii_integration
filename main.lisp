; global variables for can info
(def bus_voltage 0)

(def accelerator 0)
; 0-eco, 2-sport, 3-snow
(def drive_mode 0)
; 0-neutral, 1-forward, 2-reverse
(def direction_mode 0)

; 0 is no signal, 1 is ON I, 2 is ON II, 3 is ON III (temporary - only for starting)
(def ignition_status 0)
(def turned_on 0)


(def last_can_update 0)



; basically the function called at every can-sid-message event
(defun proc-sid (id data)
    (progn

    ; 0x1B0 is TX PDO 1 of the Led Panel - contains Drive Mode selector
    (if (= id 432)
        (progn
        
        ; get drive mode
        (def drive_mode (bufget-u8 data 0))
        
        ; only allow setting drive mode to forwards if speed is above -2m/s
        (if (> (get-speed) -2)
            (progn
            (if (= (bufget-u8 data 4) 1)
                (def direction_mode 1)
            )
        ))
        ; only allow setting drive mode to backwards if speed is below 2m/s
        (if (< (get-speed) 2)
            (progn
            (if (= (bufget-u8 data 4) 4)
                (def direction_mode 2)
            )
        ))
        
        ; always allow drive mode to be set to neutral
        (if (= (bufget-u8 data 4) 2)
            (def direction_mode 0)
        )
        
        )
    )


    ; 0x291 is TX PDO 1 of the ECU - contains accelerator, brake and some other info
    (if (= id 657)
        (progn
        
        ; get bus voltage and multiply by LSB value given in documentation
        (def bus_voltage (* (bufget-u8 data 5) 0.05633))
        
        ; get values for accelerator pedal position and adjust them to fit inside the 0 to 1 range
        ; minimal value for this is usually 33 and max is 203
        (def accelerator (* (- (bufget-u8 data 1) 35) 0.61 0.01))
        
        ; ignition status - 128, 192, 252
        (def ignition_status (bufget-u8 data 4))
        
        ; turn car on when ignition is on stage 3
        (if (= ignition_status 252) (def turned_on 1))
        ; turn car off if ignition stage 2 is left
        (if (= ignition_status 128) (def turned_on 0))
        
        
        
        ; kill car if turned off
        (if (= turned_on 0) (def accelerator 0))
        
        ; turn off accelerator if bus voltage is too low - brown out protection
        (if (< bus_voltage 9) (def accelerator 0))
        
        ; cap off the safety margin
        (if (< accelerator 0) (def accelerator 0))
        (if (> accelerator 1) (def accelerator 1))
        
        
        (if (= direction_mode 0) (set-current-rel 0 0.2))
        (if (= direction_mode 1) (set-current-rel accelerator 0.2))
        (if (= direction_mode 2) (set-current-rel (* accelerator -1) 0.2))
        
        
        ; reset can update timer
        (def last_can_update (systime))
        
        )
    )
    
    
    
    ; save memory, kids
    (free data)
    
    )
)

(defun event-handler ()
    (loopwhile t
        (recv
            ((event-can-sid (? id) . (? data)) (proc-sid id data))
            (_ nil) ; Ignore other events
)))

; Spawn the event handler thread and pass the ID it returns to C
(event-register-handler (spawn event-handler))

; Enable the CAN event for standard ID (SID) frames
(event-enable 'event-can-sid)



; windows xp startup sound
(foc-beep 622 0.35 3)
(foc-beep 466 0.25 2.5)
(foc-beep 311 0.45 1.5)
(foc-beep 622 0.3 3)
(foc-beep 466 0.4 2.5)




; loop for can sync-loss detection
(defun can_sync_loss_detection ()
    (loopwhile t
        (progn
        ; if last can update is older than 200ms
        (if (> (- (systime) last_can_update) 2000)
            (progn
            (set-current-rel 0 0.05)
            (print "can_sync loss")
        ))
        (sleep 0.01)
    ))
)

(spawn 100 can_sync_loss_detection)
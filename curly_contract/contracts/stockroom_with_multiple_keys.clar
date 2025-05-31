;; Enhanced Multi-Key Storage Smart Contract
;; Advanced storage contract with comprehensive features and access control

;; Define the main storage map for key-value pairs
(define-map storage 
  { key: (string-ascii 64) }
  { 
    value: (string-ascii 256),
    created-at: uint,
    updated-at: uint,
    access-count: uint,
    is-locked: bool
  }
)

;; Map to track authorized users who can modify specific keys
(define-map key-permissions
  { key: (string-ascii 64), user: principal }
  { can-read: bool, can-write: bool }
)

;; Map to track global user permissions
(define-map user-roles
  { user: principal }
  { role: (string-ascii 20), is-active: bool }
)

;; Map for storing metadata about keys
(define-map key-metadata
  { key: (string-ascii 64) }
  { 
    description: (string-ascii 128),
    category: (string-ascii 32),
    tags: (list 5 (string-ascii 20))
  }
)

;; Contract configuration
(define-map config
  { setting: (string-ascii 32) }
  { value: (string-ascii 64) }
)

;; Event logging
(define-map event-log
  { event-id: uint }
  {
    event-type: (string-ascii 32),
    key: (string-ascii 64),
    user: principal,
    timestamp: uint,
    details: (string-ascii 128)
  }
)

;; Error constants
(define-constant ERR-KEY-NOT-FOUND (err u100))
(define-constant ERR-UNAUTHORIZED (err u101))
(define-constant ERR-INVALID-KEY (err u102))
(define-constant ERR-KEY-LOCKED (err u103))
(define-constant ERR-USER-NOT-FOUND (err u104))
(define-constant ERR-INVALID-ROLE (err u105))
(define-constant ERR-KEY-EXISTS (err u106))
(define-constant ERR-INVALID-INPUT (err u107))
(define-constant ERR-QUOTA-EXCEEDED (err u108))

;; Role constants
(define-constant ROLE-ADMIN "admin")
(define-constant ROLE-EDITOR "editor")
(define-constant ROLE-VIEWER "viewer")

;; Contract deployer (owner)
(define-constant CONTRACT-OWNER tx-sender)

;; Data variables
(define-data-var next-event-id uint u1)
(define-data-var total-keys uint u0)
(define-data-var contract-paused bool false)

;; Authorization helpers
(define-private (is-admin (user principal))
  (match (map-get? user-roles { user: user })
    some-role (and (get is-active some-role) (is-eq (get role some-role) ROLE-ADMIN))
    false
  )
)

(define-private (is-editor-or-admin (user principal))
  (match (map-get? user-roles { user: user })
    some-role (and 
      (get is-active some-role) 
      (or (is-eq (get role some-role) ROLE-ADMIN) (is-eq (get role some-role) ROLE-EDITOR))
    )
    false
  )
)

(define-private (can-write-key (user principal) (key (string-ascii 64)))
  (or 
    (is-eq user CONTRACT-OWNER)
    (is-admin user)
    (match (map-get? key-permissions { key: key, user: user })
      some-perm (get can-write some-perm)
      false
    )
  )
)

(define-private (can-read-key (user principal) (key (string-ascii 64)))
  (or 
    (is-eq user CONTRACT-OWNER)
    (is-some (map-get? user-roles { user: user }))
    (match (map-get? key-permissions { key: key, user: user })
      some-perm (get can-read some-perm)
      false
    )
  )
)

;; Event logging helper
(define-private (log-event (event-type (string-ascii 32)) (key (string-ascii 64)) (details (string-ascii 128)))
  (let ((event-id (var-get next-event-id)))
    (map-set event-log 
      { event-id: event-id }
      {
        event-type: event-type,
        key: key,
        user: tx-sender,
        timestamp: block-height,
        details: details
      }
    )
    (var-set next-event-id (+ event-id u1))
  )
)

;; Core storage functions
(define-public (set-value (key (string-ascii 64)) (value (string-ascii 256)))
  (begin
    (asserts! (not (var-get contract-paused)) ERR-UNAUTHORIZED)
    (asserts! (can-write-key tx-sender key) ERR-UNAUTHORIZED)
    (asserts! (> (len key) u0) ERR-INVALID-KEY)
    (asserts! (<= (len value) u256) ERR-INVALID-INPUT)
    
    ;; Check if key exists and is locked
    (let ((existing-entry (map-get? storage { key: key })))
      ;; If key exists, check if it's locked
      (if (is-some existing-entry)
        (asserts! (not (get is-locked (unwrap-panic existing-entry))) ERR-KEY-LOCKED)
        true ;; New key, not locked
      )
      
      (let ((current-time block-height)
            (is-new-key (is-none existing-entry)))
        
        ;; Update storage
        (map-set storage 
          { key: key } 
          { 
            value: value,
            created-at: (if is-new-key 
              current-time 
              (get created-at (unwrap-panic existing-entry))),
            updated-at: current-time,
            access-count: u0,
            is-locked: false
          }
        )
        
        ;; Update total keys counter for new keys
        (if is-new-key (var-set total-keys (+ (var-get total-keys) u1)) true)
        
        ;; Log event
        (log-event "SET_VALUE" key (concat "Value set: " (if is-new-key "new" "updated")))
        
        (ok true)
      )
    )
  )
)

(define-read-only (get-value (key (string-ascii 64)))
  (begin
    (asserts! (can-read-key tx-sender key) ERR-UNAUTHORIZED)
    (match (map-get? storage { key: key })
      some-value (begin
        ;; Increment access count (note: this is read-only, so we can't actually update)
        (ok {
          value: (get value some-value),
          created-at: (get created-at some-value),
          updated-at: (get updated-at some-value),
          access-count: (get access-count some-value),
          is-locked: (get is-locked some-value)
        })
      )
      ERR-KEY-NOT-FOUND
    )
  )
)

;; Enhanced key management
(define-public (create-key-with-metadata 
  (key (string-ascii 64)) 
  (value (string-ascii 256))
  (description (string-ascii 128))
  (category (string-ascii 32))
  (tags (list 5 (string-ascii 20))))
  (begin
    (asserts! (is-none (map-get? storage { key: key })) ERR-KEY-EXISTS)
    (try! (set-value key value))
    
    ;; Set metadata
    (map-set key-metadata
      { key: key }
      {
        description: description,
        category: category,
        tags: tags
      }
    )
    
    (log-event "CREATE_KEY" key "Key created with metadata")
    (ok true)
  )
)

(define-public (lock-key (key (string-ascii 64)))
  (begin
    (asserts! (can-write-key tx-sender key) ERR-UNAUTHORIZED)
    (match (map-get? storage { key: key })
      some-entry (begin
        (map-set storage
          { key: key }
          (merge some-entry { is-locked: true })
        )
        (log-event "LOCK_KEY" key "Key locked")
        (ok true)
      )
      ERR-KEY-NOT-FOUND
    )
  )
)

(define-public (unlock-key (key (string-ascii 64)))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (match (map-get? storage { key: key })
      some-entry (begin
        (map-set storage
          { key: key }
          (merge some-entry { is-locked: false })
        )
        (log-event "UNLOCK_KEY" key "Key unlocked")
        (ok true)
      )
      ERR-KEY-NOT-FOUND
    )
  )
)

;; Batch operations
(define-public (batch-set-values (entries (list 10 { key: (string-ascii 64), value: (string-ascii 256) })))
  (begin
    (asserts! (is-editor-or-admin tx-sender) ERR-UNAUTHORIZED)
    (fold batch-set-helper entries (ok u0))
  )
)

(define-private (batch-set-helper 
  (entry { key: (string-ascii 64), value: (string-ascii 256) }) 
  (prev-result (response uint uint)))
  (match prev-result
    ok-val (match (set-value (get key entry) (get value entry))
      ok-result (ok (+ ok-val u1))
      err-result (err err-result)
    )
    err-val (err err-val)
  )
)

;; User and permission management
(define-public (add-user (user principal) (role (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (or (is-eq role ROLE-ADMIN) (is-eq role ROLE-EDITOR) (is-eq role ROLE-VIEWER)) ERR-INVALID-ROLE)
    
    (map-set user-roles
      { user: user }
      { role: role, is-active: true }
    )
    
    (log-event "ADD_USER" "" (concat "User added with role: " role))
    (ok true)
  )
)

(define-public (set-key-permission 
  (key (string-ascii 64)) 
  (user principal) 
  (can-read bool) 
  (can-write bool))
  (begin
    (asserts! (can-write-key tx-sender key) ERR-UNAUTHORIZED)
    
    (map-set key-permissions
      { key: key, user: user }
      { can-read: can-read, can-write: can-write }
    )
    
    (log-event "SET_PERMISSION" key "Key permissions updated")
    (ok true)
  )
)

;; Search and query functions
(define-read-only (search-keys-by-category (category (string-ascii 32)))
  (ok category) ;; Simplified - in practice would need iteration
)

(define-read-only (get-key-metadata (key (string-ascii 64)))
  (match (map-get? key-metadata { key: key })
    some-metadata (ok some-metadata)
    ERR-KEY-NOT-FOUND
  )
)

;; Analytics and reporting
(define-read-only (get-contract-stats)
  (ok {
    total-keys: (var-get total-keys),
    total-events: (- (var-get next-event-id) u1),
    is-paused: (var-get contract-paused)
  })
)

(define-read-only (get-event-log (event-id uint))
  (match (map-get? event-log { event-id: event-id })
    some-event (ok some-event)
    ERR-KEY-NOT-FOUND
  )
)

;; Contract administration
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set contract-paused true)
    (log-event "PAUSE_CONTRACT" "" "Contract paused")
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set contract-paused false)
    (log-event "UNPAUSE_CONTRACT" "" "Contract unpaused")
    (ok true)
  )
)

(define-public (set-config (setting (string-ascii 32)) (value (string-ascii 64)))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (map-set config { setting: setting } { value: value })
    (log-event "SET_CONFIG" "" (concat "Config updated: " setting))
    (ok true)
  )
)

(define-read-only (get-config (setting (string-ascii 32)))
  (match (map-get? config { setting: setting })
    some-config (ok (get value some-config))
    ERR-KEY-NOT-FOUND
  )
)

;; Utility functions
(define-read-only (key-exists? (key (string-ascii 64)))
  (is-some (map-get? storage { key: key }))
)

(define-read-only (get-contract-owner)
  CONTRACT-OWNER
)

(define-read-only (get-user-role (user principal))
  (match (map-get? user-roles { user: user })
    some-role (ok some-role)
    ERR-USER-NOT-FOUND
  )
)

;; Initialization function
(define-public (initialize-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    
    ;; Set default config
    (try! (set-config "max-keys-per-user" "100"))
    (try! (set-config "default-key-ttl" "0"))
    (try! (set-config "enable-public-read" "false"))
    
    ;; Add owner as admin
    (try! (add-user CONTRACT-OWNER ROLE-ADMIN))
    
    ;; Create initial demo data
    (try! (create-key-with-metadata 
      "app-name" 
      "Enhanced Multi-Key Storage" 
      "Application name and branding"
      "system"
      (list "app" "name" "system")
    ))
    
    (log-event "INITIALIZE" "" "Contract initialized successfully")
    (ok "Contract initialized with enhanced features")
  )
)
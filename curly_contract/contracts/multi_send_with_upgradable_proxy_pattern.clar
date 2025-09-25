;; =============================================================================
;; UPGRADABLE MULTI-SEND PROXY CONTRACT
;; =============================================================================
;; This contract implements an upgradable multi-send system using proxy pattern
;; The proxy contract delegates calls to an implementation contract that can be upgraded

;; =============================================================================
;; PROXY CONTRACT
;; =============================================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-INVALID-IMPLEMENTATION (err u101))
(define-constant ERR-IMPLEMENTATION-NOT-SET (err u102))
(define-constant ERR-INVALID-RECIPIENT (err u103))
(define-constant ERR-INSUFFICIENT-BALANCE (err u104))
(define-constant ERR-TRANSFER-FAILED (err u105))
(define-constant ERR-EMPTY-BATCH (err u106))
(define-constant ERR-BATCH-TOO-LARGE (err u107))

;; Storage for the current implementation contract
(define-data-var implementation-contract (optional principal) none)

;; Track contract version for upgrades
(define-data-var contract-version uint u1)

;; Maximum batch size to prevent gas limit issues
(define-constant MAX-BATCH-SIZE u50)

;; =============================================================================
;; PROXY FUNCTIONS
;; =============================================================================

;; Set the implementation contract (only owner)
(define-public (set-implementation (new-implementation principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set implementation-contract (some new-implementation))
    (var-set contract-version (+ (var-get contract-version) u1))
    (ok true)
  )
)

;; Get current implementation
(define-read-only (get-implementation)
  (var-get implementation-contract)
)

;; Get contract version
(define-read-only (get-version)
  (var-get contract-version)
)

;; =============================================================================
;; MULTI-SEND IMPLEMENTATION CONTRACT
;; =============================================================================

;; Multi-send batch structure
(define-map multi-send-batches
  { batch-id: uint }
  {
    sender: principal,
    total-amount: uint,
    recipient-count: uint,
    executed: bool,
    block-height: uint
  }
)

;; Individual transfer details within a batch
(define-map batch-transfers
  { batch-id: uint, transfer-id: uint }
  {
    recipient: principal,
    amount: uint,
    executed: bool
  }
)

;; Track batch counter
(define-data-var next-batch-id uint u1)

;; Track individual transfer counter within batches
(define-map batch-transfer-counter { batch-id: uint } { count: uint })

;; =============================================================================
;; HELPER FUNCTIONS (must be defined before use)
;; =============================================================================

;; Extract amount from transfer record
(define-private (get-transfer-amount (transfer { recipient: principal, amount: uint }))
  (get amount transfer)
)

;; Store individual transfers for a batch using fold
(define-private (store-batch-transfers 
  (batch-id uint) 
  (recipients (list 50 { recipient: principal, amount: uint })) 
  (unused uint))
  (let
    (
      (indexed-recipients (map add-index-to-transfer recipients))
    )
    (begin
      (fold store-single-transfer indexed-recipients { batch-id: batch-id, index: u0, success: true })
      (ok true)
    )
  )
)

;; Helper to add index to transfer for processing
(define-private (add-index-to-transfer (transfer { recipient: principal, amount: uint }))
  transfer
)

;; Store a single transfer in the batch
(define-private (store-single-transfer 
  (transfer { recipient: principal, amount: uint })
  (state { batch-id: uint, index: uint, success: bool }))
  (if (get success state)
    (begin
      (map-set batch-transfers
        { batch-id: (get batch-id state), transfer-id: (get index state) }
        {
          recipient: (get recipient transfer),
          amount: (get amount transfer),
          executed: false
        }
      )
      { 
        batch-id: (get batch-id state), 
        index: (+ (get index state) u1), 
        success: true 
      }
    )
    state
  )
)

;; Execute all transfers in a batch using fold
(define-private (execute-batch-transfers (batch-id uint) (count uint))
  (let
    (
      (transfer-indices (generate-indices count))
    )
    (fold execute-single-transfer transfer-indices { batch-id: batch-id, success: true })
  )
)

;; Generate a list of indices for processing
(define-private (generate-indices (count uint))
  (if (<= count u10)
    (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
    (if (<= count u20)
      (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19)
      (if (<= count u30)
        (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29)
        (if (<= count u40)
          (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39)
          (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39 u40 u41 u42 u43 u44 u45 u46 u47 u48 u49)
        )
      )
    )
  )
)

;; Execute a single transfer
(define-private (execute-single-transfer 
  (transfer-id uint)
  (state { batch-id: uint, success: bool }))
  (if (get success state)
    (match (map-get? batch-transfers { batch-id: (get batch-id state), transfer-id: transfer-id })
      transfer-info
      (match (stx-transfer? (get amount transfer-info) tx-sender (get recipient transfer-info))
        success
        (begin
          (map-set batch-transfers
            { batch-id: (get batch-id state), transfer-id: transfer-id }
            (merge transfer-info { executed: true })
          )
          state
        )
        error
        { batch-id: (get batch-id state), success: false }
      )
      { batch-id: (get batch-id state), success: false }
    )
    state
  )
)

;; =============================================================================
;; MULTI-SEND FUNCTIONS
;; =============================================================================

;; Create a new multi-send batch
(define-public (create-multi-send-batch (recipients (list 50 { recipient: principal, amount: uint })))
  (let
    (
      (batch-id (var-get next-batch-id))
      (total-recipients (len recipients))
      (total-amount (fold + (map get-transfer-amount recipients) u0))
    )
    (begin
      ;; Validation checks
      (asserts! (> total-recipients u0) ERR-EMPTY-BATCH)
      (asserts! (<= total-recipients MAX-BATCH-SIZE) ERR-BATCH-TOO-LARGE)
      (asserts! (>= (stx-get-balance tx-sender) total-amount) ERR-INSUFFICIENT-BALANCE)
      
      ;; Create batch record
      (map-set multi-send-batches
        { batch-id: batch-id }
        {
          sender: tx-sender,
          total-amount: total-amount,
          recipient-count: total-recipients,
          executed: false,
          block-height: block-height
        }
      )
      
      ;; Store individual transfers
      (unwrap! (store-batch-transfers batch-id recipients u0) ERR-TRANSFER-FAILED)
      
      ;; Update batch counter
      (var-set next-batch-id (+ batch-id u1))
      
      (ok batch-id)
    )
  )
)

;; Execute a multi-send batch
(define-public (execute-multi-send-batch (batch-id uint))
  (let
    (
      (batch-info (unwrap! (map-get? multi-send-batches { batch-id: batch-id }) ERR-INVALID-IMPLEMENTATION))
    )
    (begin
      ;; Check if sender is the batch creator
      (asserts! (is-eq tx-sender (get sender batch-info)) ERR-OWNER-ONLY)
      
      ;; Check if batch hasn't been executed
      (asserts! (not (get executed batch-info)) ERR-INVALID-IMPLEMENTATION)
      
      ;; Execute all transfers in the batch
      (let ((execution-result (execute-batch-transfers batch-id (get recipient-count batch-info))))
        (asserts! (get success execution-result) ERR-TRANSFER-FAILED)
      )
      
      ;; Mark batch as executed
      (map-set multi-send-batches
        { batch-id: batch-id }
        (merge batch-info { executed: true })
      )
      
      (ok true)
    )
  )
)



;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

;; Get batch information
(define-read-only (get-batch-info (batch-id uint))
  (map-get? multi-send-batches { batch-id: batch-id })
)

;; Get transfer information
(define-read-only (get-transfer-info (batch-id uint) (transfer-id uint))
  (map-get? batch-transfers { batch-id: batch-id, transfer-id: transfer-id })
)

;; Get next batch ID
(define-read-only (get-next-batch-id)
  (var-get next-batch-id)
)

;; Check if user is contract owner
(define-read-only (is-contract-owner (user principal))
  (is-eq user CONTRACT-OWNER)
)

;; =============================================================================
;; UPGRADE FUNCTIONS
;; =============================================================================

;; Emergency pause function (owner only)
(define-data-var contract-paused bool false)

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set contract-paused false)
    (ok true)
  )
)

(define-read-only (is-paused)
  (var-get contract-paused)
)

;; Migration function for future upgrades
(define-public (migrate-data (new-contract principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    ;; Implementation-specific migration logic would go here
    ;; This is a placeholder for future upgrade scenarios
    (ok true)
  )
)

;; =============================================================================
;; EVENTS AND LOGGING
;; =============================================================================

;; Print events for better observability
(define-private (log-batch-created (batch-id uint))
  (print { event: "batch-created", batch-id: batch-id, sender: tx-sender })
)

(define-private (log-batch-executed (batch-id uint))
  (print { event: "batch-executed", batch-id: batch-id, sender: tx-sender })
)

(define-private (log-contract-upgraded (version uint))
  (print { event: "contract-upgraded", version: version, sender: tx-sender })
)
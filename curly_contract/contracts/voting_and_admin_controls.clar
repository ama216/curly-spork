;; Enhanced Voting Contract with Advanced Features
;; Comprehensive voting system with multiple admin controls and advanced functionality

;; Constants
(define-constant CONTRACT_ADMIN tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_VOTING_NOT_ACTIVE (err u101))
(define-constant ERR_VOTING_ACTIVE (err u102))
(define-constant ERR_CANDIDATE_NOT_FOUND (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_CANDIDATE_EXISTS (err u105))
(define-constant ERR_VOTING_ENDED (err u106))
(define-constant ERR_INVALID_TIME (err u107))
(define-constant ERR_MINIMUM_CANDIDATES (err u108))
(define-constant ERR_VOTER_NOT_REGISTERED (err u109))
(define-constant ERR_VOTER_ALREADY_REGISTERED (err u110))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u111))
(define-constant ERR_INSUFFICIENT_STAKE (err u112))
(define-constant ERR_VOTING_TOO_EARLY (err u113))
(define-constant ERR_VOTING_TOO_LATE (err u114))
(define-constant ERR_INVALID_VOTE_WEIGHT (err u115))

;; Data Variables
(define-data-var voting-active bool false)
(define-data-var candidate-counter uint u0)
(define-data-var total-votes uint u0)
(define-data-var voting-start-time uint u0)
(define-data-var voting-end-time uint u0)
(define-data-var minimum-candidates uint u2)
(define-data-var require-registration bool false)
(define-data-var allow-vote-changes bool false)
(define-data-var voting-fee uint u0)
(define-data-var winner-candidate-id uint u0)
(define-data-var voting-concluded bool false)
(define-data-var proposal-counter uint u0)
(define-data-var voting-title (string-ascii 100) "")
(define-data-var voting-description (string-ascii 500) "")

;; Data Maps
(define-map candidates 
  { candidate-id: uint }
  { 
    name: (string-ascii 50),
    description: (string-ascii 200),
    vote-count: uint,
    weighted-vote-count: uint,
    active: bool,
    added-at: uint
  }
)

(define-map voter-records
  { voter: principal }
  { 
    has-voted: bool,
    candidate-voted-for: uint,
    vote-weight: uint,
    voted-at: uint,
    stake-amount: uint
  }
)

(define-map registered-voters
  { voter: principal }
  {
    registered: bool,
    registration-time: uint,
    vote-weight: uint
  }
)

(define-map candidate-names
  { name: (string-ascii 50) }
  { candidate-id: uint }
)

(define-map vote-history
  { voter: principal, vote-number: uint }
  {
    candidate-id: uint,
    timestamp: uint,
    vote-weight: uint
  }
)

(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    votes-for: uint,
    votes-against: uint,
    active: bool,
    created-at: uint
  }
)

(define-map proposal-votes
  { voter: principal, proposal-id: uint }
  { support: bool, weight: uint }
)

(define-map admin-permissions
  { admin: principal }
  { 
    can-add-candidates: bool,
    can-manage-voting: bool,
    can-register-voters: bool,
    active: bool
  }
)

(define-map voting-statistics
  { key: (string-ascii 20) }
  { value: uint }
)

;; Private Functions

(define-private (is-admin (user principal))
  (or 
    (is-eq user CONTRACT_ADMIN)
    (default-to false (get active (map-get? admin-permissions { admin: user })))
  )
)

(define-private (has-permission (user principal) (permission (string-ascii 20)))
  (if (is-eq user CONTRACT_ADMIN)
    true
    (match (map-get? admin-permissions { admin: user })
      admin-data
      (and 
        (get active admin-data)
        (if (is-eq permission "add-candidates")
          (get can-add-candidates admin-data)
          (if (is-eq permission "manage-voting")
            (get can-manage-voting admin-data)
            (if (is-eq permission "register-voters")
              (get can-register-voters admin-data)
              false))))
      false)
  )
)

(define-private (update-statistics (key (string-ascii 20)) (value uint))
  (map-set voting-statistics { key: key } { value: value })
)

(define-private (increment-statistic (key (string-ascii 20)))
  (let ((current-value (default-to u0 (get value (map-get? voting-statistics { key: key })))))
    (update-statistics key (+ current-value u1))
  )
)

(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b)
)

;; Read-only Functions

(define-read-only (get-admin)
  CONTRACT_ADMIN
)

(define-read-only (is-voting-active)
  (and 
    (var-get voting-active)
    (not (var-get voting-concluded))
    (>= block-height (var-get voting-start-time))
    (<= block-height (var-get voting-end-time))
  )
)

(define-read-only (get-voting-info)
  {
    active: (var-get voting-active),
    concluded: (var-get voting-concluded),
    start-time: (var-get voting-start-time),
    end-time: (var-get voting-end-time),
    total-votes: (var-get total-votes),
    candidate-count: (var-get candidate-counter),
    title: (var-get voting-title),
    description: (var-get voting-description),
    winner: (var-get winner-candidate-id)
  }
)

(define-read-only (get-candidate (candidate-id uint))
  (map-get? candidates { candidate-id: candidate-id })
)

(define-read-only (get-all-candidates)
  (let ((total-candidates (var-get candidate-counter)))
    (map get-candidate-with-id (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20))
  )
)

(define-private (get-candidate-with-id (candidate-id uint))
  (if (<= candidate-id (var-get candidate-counter))
    (match (map-get? candidates { candidate-id: candidate-id })
      candidate-data (some {
        candidate-id: candidate-id,
        name: (get name candidate-data),
        description: (get description candidate-data),
        vote-count: (get vote-count candidate-data),
        weighted-vote-count: (get weighted-vote-count candidate-data),
        active: (get active candidate-data),
        added-at: (get added-at candidate-data)
      })
      none)
    none
  )
)

(define-read-only (get-candidate-count)
  (var-get candidate-counter)
)

(define-read-only (has-voted (voter principal))
  (default-to false (get has-voted (map-get? voter-records { voter: voter })))
)

(define-read-only (get-voter-info (voter principal))
  (map-get? voter-records { voter: voter })
)

(define-read-only (is-registered-voter (voter principal))
  (if (var-get require-registration)
    (default-to false (get registered (map-get? registered-voters { voter: voter })))
    true
  )
)

(define-read-only (get-voting-results)
  (let ((total-candidates (var-get candidate-counter)))
    {
      candidates: (get-all-candidates),
      total-votes: (var-get total-votes),
      winner: (var-get winner-candidate-id),
      concluded: (var-get voting-concluded)
    }
  )
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-statistics)
  {
    total-registered-voters: (default-to u0 (get value (map-get? voting-statistics { key: "reg-voters" }))),
    total-votes-cast: (var-get total-votes),
    average-vote-weight: (if (> (var-get total-votes) u0)
      (/ (default-to u0 (get value (map-get? voting-statistics { key: "total-weight" }))) (var-get total-votes))
      u0),
    voting-participation: (if (> (default-to u1 (get value (map-get? voting-statistics { key: "reg-voters" }))) u0)
      (/ (* (var-get total-votes) u100) (default-to u1 (get value (map-get? voting-statistics { key: "reg-voters" }))))
      u0)
  }
)

;; Admin Functions - Enhanced

(define-public (set-voting-details (title (string-ascii 100)) (description (string-ascii 500)))
  (begin
    (asserts! (has-permission tx-sender "manage-voting") ERR_UNAUTHORIZED)
    (asserts! (not (var-get voting-active)) ERR_VOTING_ACTIVE)
    
    (var-set voting-title title)
    (var-set voting-description description)
    (ok true)
  )
)

(define-public (add-candidate (name (string-ascii 50)) (description (string-ascii 200)))
  (begin
    (asserts! (has-permission tx-sender "add-candidates") ERR_UNAUTHORIZED)
    (asserts! (not (var-get voting-active)) ERR_VOTING_ACTIVE)
    (asserts! (is-none (map-get? candidate-names { name: name })) ERR_CANDIDATE_EXISTS)
    
    (var-set candidate-counter (+ (var-get candidate-counter) u1))
    
    (let ((new-candidate-id (var-get candidate-counter)))
      (map-set candidates 
        { candidate-id: new-candidate-id }
        { 
          name: name,
          description: description,
          vote-count: u0,
          weighted-vote-count: u0,
          active: true,
          added-at: block-height
        }
      )
      
      (map-set candidate-names
        { name: name }
        { candidate-id: new-candidate-id }
      )
      
      (ok new-candidate-id)
    )
  )
)

(define-public (deactivate-candidate (candidate-id uint))
  (begin
    (asserts! (has-permission tx-sender "add-candidates") ERR_UNAUTHORIZED)
    (asserts! (not (var-get voting-active)) ERR_VOTING_ACTIVE)
    
    (match (map-get? candidates { candidate-id: candidate-id })
      candidate-data
      (begin
        (map-set candidates
          { candidate-id: candidate-id }
          (merge candidate-data { active: false })
        )
        (ok true)
      )
      ERR_CANDIDATE_NOT_FOUND
    )
  )
)

(define-public (start-voting-with-duration (duration-blocks uint))
  (begin
    (asserts! (has-permission tx-sender "manage-voting") ERR_UNAUTHORIZED)
    (asserts! (not (var-get voting-active)) ERR_VOTING_ACTIVE)
    (asserts! (>= (var-get candidate-counter) (var-get minimum-candidates)) ERR_MINIMUM_CANDIDATES)
    (asserts! (> duration-blocks u0) ERR_INVALID_TIME)
    
    (var-set voting-start-time block-height)
    (var-set voting-end-time (+ block-height duration-blocks))
    (var-set voting-active true)
    (var-set voting-concluded false)
    
    (ok { start: block-height, end: (+ block-height duration-blocks) })
  )
)

(define-public (start-voting)
  (start-voting-with-duration u1008) ;; Default 1 week (assuming 10 min blocks)
)

(define-public (end-voting)
  (begin
    (asserts! (has-permission tx-sender "manage-voting") ERR_UNAUTHORIZED)
    (asserts! (var-get voting-active) ERR_VOTING_NOT_ACTIVE)
    
    (var-set voting-active false)
    (var-set voting-concluded true)
    
    ;; Determine winner
    (let ((winner-result (get-winner)))
      (let ((winner-id (get winner winner-result)))
        (var-set winner-candidate-id winner-id)
        (ok { winner: winner-id, total-votes: (var-get total-votes) })
      )
    )
  )
)

(define-public (extend-voting (additional-blocks uint))
  (begin
    (asserts! (has-permission tx-sender "manage-voting") ERR_UNAUTHORIZED)
    (asserts! (var-get voting-active) ERR_VOTING_NOT_ACTIVE)
    
    (var-set voting-end-time (+ (var-get voting-end-time) additional-blocks))
    (ok (var-get voting-end-time))
  )
)

(define-public (register-voter (voter principal) (vote-weight uint))
  (begin
    (asserts! (has-permission tx-sender "register-voters") ERR_UNAUTHORIZED)
    (asserts! (> vote-weight u0) ERR_INVALID_VOTE_WEIGHT)
    (asserts! (is-none (map-get? registered-voters { voter: voter })) ERR_VOTER_ALREADY_REGISTERED)
    
    (map-set registered-voters
      { voter: voter }
      {
        registered: true,
        registration-time: block-height,
        vote-weight: vote-weight
      }
    )
    
    (increment-statistic "reg-voters")
    (ok true)
  )
)

(define-public (set-voting-requirements (require-reg bool) (min-candidates uint) (fee uint))
  (begin
    (asserts! (has-permission tx-sender "manage-voting") ERR_UNAUTHORIZED)
    (asserts! (not (var-get voting-active)) ERR_VOTING_ACTIVE)
    
    (var-set require-registration require-reg)
    (var-set minimum-candidates min-candidates)
    (var-set voting-fee fee)
    (ok true)
  )
)

(define-public (add-admin (new-admin principal) (permissions { can-add-candidates: bool, can-manage-voting: bool, can-register-voters: bool }))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_ADMIN) ERR_UNAUTHORIZED)
    
    (map-set admin-permissions
      { admin: new-admin }
      (merge permissions { active: true })
    )
    (ok true)
  )
)

(define-public (revoke-admin (admin principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_ADMIN) ERR_UNAUTHORIZED)
    
    (match (map-get? admin-permissions { admin: admin })
      admin-data
      (begin
        (map-set admin-permissions
          { admin: admin }
          (merge admin-data { active: false })
        )
        (ok true)
      )
      (ok true) ;; No admin to revoke
    )
  )
)

;; Enhanced Voting Functions

(define-public (vote (candidate-id uint))
  (vote-with-weight candidate-id u1)
)

(define-public (vote-with-weight (candidate-id uint) (weight uint))
  (begin
    (asserts! (is-voting-active) ERR_VOTING_NOT_ACTIVE)
    (asserts! (>= block-height (var-get voting-start-time)) ERR_VOTING_TOO_EARLY)
    (asserts! (<= block-height (var-get voting-end-time)) ERR_VOTING_TOO_LATE)
    
    ;; Check registration if required
    (asserts! (is-registered-voter tx-sender) ERR_VOTER_NOT_REGISTERED)
    
    ;; Check if already voted (unless changes allowed)
    (if (var-get allow-vote-changes)
      true
      (asserts! (not (has-voted tx-sender)) ERR_ALREADY_VOTED)
    )
    
    ;; Validate candidate
    (match (map-get? candidates { candidate-id: candidate-id })
      candidate-data
      (begin
        (asserts! (get active candidate-data) ERR_CANDIDATE_NOT_FOUND)
        
        ;; Determine vote weight
        (let ((final-weight (if (var-get require-registration)
          (min-uint weight (default-to u1 (get vote-weight (map-get? registered-voters { voter: tx-sender }))))
          weight)))
          
          ;; Handle vote change
          (if (has-voted tx-sender)
            (handle-vote-change candidate-id final-weight)
            (cast-new-vote candidate-id final-weight)
          )
        )
      )
      ERR_CANDIDATE_NOT_FOUND
    )
  )
)

(define-private (cast-new-vote (candidate-id uint) (weight uint))
  (let ((candidate-data (unwrap-panic (map-get? candidates { candidate-id: candidate-id }))))
    ;; Update candidate votes
    (map-set candidates
      { candidate-id: candidate-id }
      (merge candidate-data {
        vote-count: (+ (get vote-count candidate-data) u1),
        weighted-vote-count: (+ (get weighted-vote-count candidate-data) weight)
      })
    )
    
    ;; Record voter's choice
    (map-set voter-records
      { voter: tx-sender }
      {
        has-voted: true,
        candidate-voted-for: candidate-id,
        vote-weight: weight,
        voted-at: block-height,
        stake-amount: u0
      }
    )
    
    ;; Update statistics
    (var-set total-votes (+ (var-get total-votes) u1))
    (update-statistics "total-weight" (+ (default-to u0 (get value (map-get? voting-statistics { key: "total-weight" }))) weight))
    
    (ok candidate-id)
  )
)

(define-private (handle-vote-change (new-candidate-id uint) (weight uint))
  (let (
    (old-vote (unwrap-panic (map-get? voter-records { voter: tx-sender })))
    (old-candidate-id (get candidate-voted-for old-vote))
    (old-weight (get vote-weight old-vote))
  )
    ;; Remove old vote
    (let ((old-candidate (unwrap-panic (map-get? candidates { candidate-id: old-candidate-id }))))
      (map-set candidates
        { candidate-id: old-candidate-id }
        (merge old-candidate {
          vote-count: (- (get vote-count old-candidate) u1),
          weighted-vote-count: (- (get weighted-vote-count old-candidate) old-weight)
        })
      )
    )
    
    ;; Cast new vote
    (cast-new-vote new-candidate-id weight)
  )
)

;; Proposal System

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)))
  (begin
    (asserts! (is-registered-voter tx-sender) ERR_VOTER_NOT_REGISTERED)
    
    (var-set proposal-counter (+ (var-get proposal-counter) u1))
    (let ((proposal-id (var-get proposal-counter)))
      (map-set proposals
        { proposal-id: proposal-id }
        {
          title: title,
          description: description,
          proposer: tx-sender,
          votes-for: u0,
          votes-against: u0,
          active: true,
          created-at: block-height
        }
      )
      (ok proposal-id)
    )
  )
)

(define-public (vote-on-proposal (proposal-id uint) (support bool))
  (begin
    (asserts! (is-registered-voter tx-sender) ERR_VOTER_NOT_REGISTERED)
    
    (match (map-get? proposals { proposal-id: proposal-id })
      proposal-data
      (let ((vote-weight (default-to u1 (get vote-weight (map-get? registered-voters { voter: tx-sender })))))
        ;; Record vote
        (map-set proposal-votes
          { voter: tx-sender, proposal-id: proposal-id }
          { support: support, weight: vote-weight }
        )
        
        ;; Update proposal counts
        (map-set proposals
          { proposal-id: proposal-id }
          (if support
            (merge proposal-data { votes-for: (+ (get votes-for proposal-data) vote-weight) })
            (merge proposal-data { votes-against: (+ (get votes-against proposal-data) vote-weight) })
          )
        )
        (ok true)
      )
      ERR_PROPOSAL_NOT_FOUND
    )
  )
)

;; Utility Functions

(define-private (get-winner)
  (fold find-max-votes (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20) { winner: u0, max-votes: u0 })
)

(define-private (find-max-votes (candidate-id uint) (current-max { winner: uint, max-votes: uint }))
  (if (<= candidate-id (var-get candidate-counter))
    (match (map-get? candidates { candidate-id: candidate-id })
      candidate-data
      (if (> (get weighted-vote-count candidate-data) (get max-votes current-max))
        { winner: candidate-id, max-votes: (get weighted-vote-count candidate-data) }
        current-max)
      current-max)
    current-max
  )
)

(define-read-only (get-leaderboard)
  (let ((all-candidates (get-all-candidates)))
    {
      candidates: all-candidates,
      sorted-by-votes: true
    }
  )
)
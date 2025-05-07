;; creator-funding
;; Manages project funding, milestones, and verification processes for creative projects on the Stacks blockchain
;; This contract implements a milestone-based funding system where creators can register projects, 
;; supporters can contribute funds, and funds are released only upon milestone verification.
;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROJECT-NOT-FOUND (err u101))
(define-constant ERR-MILESTONE-NOT-FOUND (err u102))
(define-constant ERR-INVALID-STATUS (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-VERIFICATION-FAILED (err u105))
(define-constant ERR-ALREADY-VERIFIED (err u106))
(define-constant ERR-ALREADY-VOTED (err u107))
(define-constant ERR-NOT-CONTRIBUTOR (err u108))
(define-constant ERR-PROJECT-INCOMPLETE (err u109))
(define-constant ERR-INVALID-AMOUNT (err u110))
(define-constant ERR-ALREADY-EXISTS (err u111))
;; Project status constants
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-COMPLETED u2)
(define-constant STATUS-CANCELLED u3)
;; Milestone status constants
(define-constant MILESTONE-PENDING u1)
(define-constant MILESTONE-VERIFICATION u2)
(define-constant MILESTONE-COMPLETED u3)
(define-constant MILESTONE-REJECTED u4)
;; Verification method constants
(define-constant VERIFICATION-VOTING u1)
(define-constant VERIFICATION-DESIGNATED u2)
;; Data structures
;; Tracks basic project information
(define-map projects
  { project-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-utf8 1000),
    funding-goal: uint,
    current-funding: uint,
    verification-method: uint,
    status: uint,
    milestone-count: uint,
    created-at: uint,
  }
)
;; Tracks milestone information for each project
(define-map milestones
  {
    project-id: uint,
    milestone-id: uint,
  }
  {
    title: (string-ascii 100),
    description: (string-utf8 500),
    funding-amount: uint,
    status: uint,
    evidence: (optional (string-utf8 500)),
    verification-deadline: (optional uint),
  }
)
;; Tracks user contributions to projects
(define-map contributions
  {
    project-id: uint,
    contributor: principal,
  }
  { amount: uint }
)
;; Tracks total contribution amount by contributor
(define-map contributor-totals
  { contributor: principal }
  { total-contributed: uint }
)
;; Tracks verification votes for milestones (for voting-based verification)
(define-map verification-votes
  {
    project-id: uint,
    milestone-id: uint,
    voter: principal,
  }
  { approved: bool }
)
;; Tracks vote tallies for milestone verification
(define-map vote-counts
  {
    project-id: uint,
    milestone-id: uint,
  }
  {
    approve-count: uint,
    reject-count: uint,
    verified: bool,
  }
)
;; Tracks designated verifiers for projects using designated verification
(define-map designated-verifiers
  {
    project-id: uint,
    verifier: principal,
  }
  { active: bool }
)
;; Global counters
(define-data-var project-id-counter uint u0)
;; Private functions
;; Generate a new project ID
(define-private (generate-project-id)
  (let ((new-id (+ (var-get project-id-counter) u1)))
    (var-set project-id-counter new-id)
    new-id
  )
)

;; Check if sender is the project creator
(define-private (is-project-creator (project-id uint))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) false)))
    (is-eq tx-sender (get creator project))
  )
)

;; Check if a project exists
(define-private (project-exists (project-id uint))
  (is-some (map-get? projects { project-id: project-id }))
)

;; Check if a milestone exists
(define-private (milestone-exists
    (project-id uint)
    (milestone-id uint)
  )
  (is-some (map-get? milestones {
    project-id: project-id,
    milestone-id: milestone-id,
  }))
)

;; Check if user has contributed to a project
(define-private (is-contributor
    (project-id uint)
    (user principal)
  )
  (is-some (map-get? contributions {
    project-id: project-id,
    contributor: user,
  }))
)

;; Check if all milestones are completed for a project
(define-private (all-milestones-completed (project-id uint))
  (let (
      (project (unwrap! (map-get? projects { project-id: project-id }) false))
      (milestone-count (get milestone-count project))
    )
    (and
      (> milestone-count u0)
      (fold check-all-milestones-complete
        (list (list milestone-count project-id)) true
      )
    )
  )
)

;; Helper for checking if all milestones are complete
(define-private (check-all-milestones-complete
    (milestone-data (list 2 uint))
    (all-complete bool)
  )
  (let (
      (milestone-count (unwrap-panic (element-at milestone-data u0)))
      (project-id (unwrap-panic (element-at milestone-data u1)))
      (milestone-id (- milestone-count u1))
      (milestone (map-get? milestones {
        project-id: project-id,
        milestone-id: milestone-id,
      }))
    )
    (match milestone
      milestone-info (and all-complete (is-eq (get status milestone-info) MILESTONE-COMPLETED))
      false
    )
  )
)

;; Public functions
;; Register a new project (Milestones are added separately)
(define-public (register-project 
  (title (string-ascii 100))
  (description (string-utf8 1000))
  (verification-method uint)
  (milestone-count uint) ;; Now expect the count directly
  (total-funding-goal uint) ;; Now expect the total goal directly
)
  (let (
    (new-id (generate-project-id))
  )
    ;; Validate inputs
    (asserts! (and (> milestone-count u0) (< milestone-count u21)) (err u112)) ;; Keep count validation
    (asserts! (or (is-eq verification-method VERIFICATION-VOTING) 
                 (is-eq verification-method VERIFICATION-DESIGNATED)) (err u113))
    (asserts! (> total-funding-goal u0) ERR-INVALID-AMOUNT) ;; Validate funding goal
    
    ;; Create project
    (map-set projects
      { project-id: new-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        funding-goal: total-funding-goal, ;; Use provided total goal
        current-funding: u0,
        verification-method: verification-method,
        status: STATUS-ACTIVE,
        milestone-count: milestone-count, ;; Store the expected count
        created-at: block-height
      }
    )
    
    ;; Milestones are no longer created here
    
    (ok new-id)
  )
)

;; Add a single milestone to a project (callable by creator)
(define-public (add-milestone
  (project-id uint)
  (milestone-id uint)
  (title (string-ascii 100))
  (description (string-utf8 500))
  (funding-amount uint)
)
  (begin
    ;; Validate project exists and sender is creator
    (asserts! (project-exists project-id) ERR-PROJECT-NOT-FOUND)
    (asserts! (is-project-creator project-id) ERR-NOT-AUTHORIZED)
    
    (let ((project (unwrap-panic (map-get? projects { project-id: project-id }))))
      ;; Validate milestone ID is within expected range (0 to count-1)
      (asserts! (< milestone-id (get milestone-count project)) (err u116)) ;; New error code needed? Using temp u116
      ;; Validate milestone doesn't already exist
      (asserts! (not (milestone-exists project-id milestone-id)) ERR-ALREADY-EXISTS)
      ;; Validate project is active
      (asserts! (is-eq (get status project) STATUS-ACTIVE) ERR-INVALID-STATUS)
    )
    
    ;; Create the milestone
    (map-set milestones
      { project-id: project-id, milestone-id: milestone-id }
      {
        title: title,
        description: description,
        funding-amount: funding-amount,
        status: MILESTONE-PENDING,
        evidence: none,
        verification-deadline: none
      }
    )
    
    (ok true)
  )
)

;; Add a designated verifier to a project (only project creator can do this)
(define-public (add-designated-verifier
    (project-id uint)
    (verifier principal)
  )
  (begin
    (asserts! (project-exists project-id) ERR-PROJECT-NOT-FOUND)
    (asserts! (is-project-creator project-id) ERR-NOT-AUTHORIZED)
    (let ((project (unwrap-panic (map-get? projects { project-id: project-id }))))
      (asserts! (is-eq (get verification-method project) VERIFICATION-DESIGNATED)
        ERR-INVALID-STATUS
      )
      (map-set designated-verifiers {
        project-id: project-id,
        verifier: verifier,
      } { active: true }
      )
      (ok true)
    )
  )
)

;; Remove a designated verifier from a project
(define-public (remove-designated-verifier
    (project-id uint)
    (verifier principal)
  )
  (begin
    (asserts! (project-exists project-id) ERR-PROJECT-NOT-FOUND)
    (asserts! (is-project-creator project-id) ERR-NOT-AUTHORIZED)
    (map-set designated-verifiers {
      project-id: project-id,
      verifier: verifier,
    } { active: false }
    )
    (ok true)
  )
)

;; Contribute funds to a project
(define-public (contribute-to-project
    (project-id uint)
    (amount uint)
  )
  (let (
      (project (unwrap! (map-get? projects { project-id: project-id })
        ERR-PROJECT-NOT-FOUND
      ))
      (current-contribution (default-to { amount: u0 }
        (map-get? contributions {
          project-id: project-id,
          contributor: tx-sender,
        })
      ))
      (current-total (default-to { total-contributed: u0 }
        (map-get? contributor-totals { contributor: tx-sender })
      ))
      (new-project-funding (+ (get current-funding project) amount))
    )
    ;; Validate contribution
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-eq (get status project) STATUS-ACTIVE) ERR-INVALID-STATUS)
    ;; Transfer STX from sender to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    ;; Update project funding
    (map-set projects { project-id: project-id }
      (merge project { current-funding: new-project-funding })
    )
    ;; Update contributor record
    (map-set contributions {
      project-id: project-id,
      contributor: tx-sender,
    } { amount: (+ (get amount current-contribution) amount) }
    )
    ;; Update contributor totals
    (map-set contributor-totals { contributor: tx-sender } { total-contributed: (+ (get total-contributed current-total) amount) })
    (ok true)
  )
)

;; Submit evidence for milestone completion (only creator can do this)
(define-public (submit-milestone-evidence
    (project-id uint)
    (milestone-id uint)
    (evidence (string-utf8 500))
  )
  (let (
      (project (unwrap! (map-get? projects { project-id: project-id })
        ERR-PROJECT-NOT-FOUND
      ))
      (milestone (unwrap!
        (map-get? milestones {
          project-id: project-id,
          milestone-id: milestone-id,
        })
        ERR-MILESTONE-NOT-FOUND
      ))
      (verification-method (get verification-method project))
    )
    ;; Validate request
    (asserts! (is-eq tx-sender (get creator project)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status milestone) MILESTONE-PENDING) ERR-INVALID-STATUS)
    ;; Set verification deadline based on method
    (let (
        (verification-deadline (if (is-eq verification-method VERIFICATION-VOTING)
          (+ block-height u144) ;; ~24 hours with 10-minute blocks
          (+ block-height u6)
        ))
        ;; ~1 hour for designated verifiers
      )
      ;; Update milestone status
      (map-set milestones {
        project-id: project-id,
        milestone-id: milestone-id,
      }
        (merge milestone {
          status: MILESTONE-VERIFICATION,
          evidence: (some evidence),
          verification-deadline: (some verification-deadline),
        })
      )
      ;; Initialize vote counts for voting method
      (if (is-eq verification-method VERIFICATION-VOTING)
        (map-set vote-counts {
          project-id: project-id,
          milestone-id: milestone-id,
        } {
          approve-count: u0,
          reject-count: u0,
          verified: false,
        })
        true
      )
      (ok true)
    )
  )
)

;; Vote on milestone verification (for voting-based verification)
(define-public (vote-on-milestone
    (project-id uint)
    (milestone-id uint)
    (approve bool)
  )
  (let (
      (project (unwrap! (map-get? projects { project-id: project-id })
        ERR-PROJECT-NOT-FOUND
      ))
      (milestone (unwrap!
        (map-get? milestones {
          project-id: project-id,
          milestone-id: milestone-id,
        })
        ERR-MILESTONE-NOT-FOUND
      ))
      (vote-key {
        project-id: project-id,
        milestone-id: milestone-id,
        voter: tx-sender,
      })
      (counts (unwrap!
        (map-get? vote-counts {
          project-id: project-id,
          milestone-id: milestone-id,
        })
        ERR-INVALID-STATUS
      ))
    )
    ;; Validate vote
    (asserts! (is-eq (get verification-method project) VERIFICATION-VOTING)
      ERR-INVALID-STATUS
    )
    (asserts! (is-eq (get status milestone) MILESTONE-VERIFICATION)
      ERR-INVALID-STATUS
    )
    (asserts! (is-contributor project-id tx-sender) ERR-NOT-CONTRIBUTOR)
    (asserts! (not (is-some (map-get? verification-votes vote-key)))
      ERR-ALREADY-VOTED
    )
    ;; Record vote
    (map-set verification-votes vote-key { approved: approve })
    ;; Update counts
    (let (
        (new-approve-count (if approve
          (+ (get approve-count counts) u1)
          (get approve-count counts)
        ))
        (new-reject-count (if approve
          (get reject-count counts)
          (+ (get reject-count counts) u1)
        ))
      )
      (map-set vote-counts {
        project-id: project-id,
        milestone-id: milestone-id,
      } {
        approve-count: new-approve-count,
        reject-count: new-reject-count,
        verified: (get verified counts),
      })
      (ok true)
    )
  )
)

;; Verify milestone as a designated verifier
(define-public (verify-milestone
    (project-id uint)
    (milestone-id uint)
    (approved bool)
  )
  (let (
      (project (unwrap! (map-get? projects { project-id: project-id })
        ERR-PROJECT-NOT-FOUND
      ))
      (milestone (unwrap!
        (map-get? milestones {
          project-id: project-id,
          milestone-id: milestone-id,
        })
        ERR-MILESTONE-NOT-FOUND
      ))
      (verifier-status (unwrap!
        (map-get? designated-verifiers {
          project-id: project-id,
          verifier: tx-sender,
        })
        ERR-NOT-AUTHORIZED
      ))
    )
    ;; Validate verification
    (asserts! (is-eq (get verification-method project) VERIFICATION-DESIGNATED)
      ERR-INVALID-STATUS
    )
    (asserts! (is-eq (get status milestone) MILESTONE-VERIFICATION)
      ERR-INVALID-STATUS
    )
    (asserts! (get active verifier-status) ERR-NOT-AUTHORIZED)
    ;; Process verification
    (if approved
      (begin
        ;; Mark as verified and release funds
        (try! (release-milestone-funds project-id milestone-id))
        (ok true)
      )
      (begin
        ;; Mark as rejected
        (map-set milestones {
          project-id: project-id,
          milestone-id: milestone-id,
        }
          (merge milestone { status: MILESTONE-REJECTED })
        )
        (ok false)
      )
    )
  )
)

;; Process voting verification (can be called by anyone after deadline)
(define-public (process-voting-verification
    (project-id uint)
    (milestone-id uint)
  )
  (let (
      (project (unwrap! (map-get? projects { project-id: project-id })
        ERR-PROJECT-NOT-FOUND
      ))
      (milestone (unwrap!
        (map-get? milestones {
          project-id: project-id,
          milestone-id: milestone-id,
        })
        ERR-MILESTONE-NOT-FOUND
      ))
      (counts (unwrap!
        (map-get? vote-counts {
          project-id: project-id,
          milestone-id: milestone-id,
        })
        ERR-INVALID-STATUS
      ))
      (deadline (unwrap! (get verification-deadline milestone) ERR-INVALID-STATUS))
    )
    ;; Validate processing
    (asserts! (is-eq (get verification-method project) VERIFICATION-VOTING)
      ERR-INVALID-STATUS
    )
    (asserts! (is-eq (get status milestone) MILESTONE-VERIFICATION)
      ERR-INVALID-STATUS
    )
    (asserts! (>= block-height deadline) ERR-INVALID-STATUS)
    (asserts! (not (get verified counts)) ERR-ALREADY-VERIFIED)
    ;; Calculate result - need more approvals than rejections
    (let (
        (approve-count (get approve-count counts))
        (reject-count (get reject-count counts))
        (is-approved (> approve-count reject-count))
      )
      ;; Mark as verified to prevent duplicate processing
      (map-set vote-counts {
        project-id: project-id,
        milestone-id: milestone-id,
      }
        (merge counts { verified: true })
      )
      ;; Process result
      (if is-approved
        (begin
          ;; Release funds
          (try! (release-milestone-funds project-id milestone-id))
          (ok true)
        )
        (begin
          ;; Mark as rejected
          (map-set milestones {
            project-id: project-id,
            milestone-id: milestone-id,
          }
            (merge milestone { status: MILESTONE-REJECTED })
          )
          (ok false)
        )
      )
    )
  )
)

;; Release funds for a verified milestone (internal function)
(define-private (release-milestone-funds
    (project-id uint)
    (milestone-id uint)
  )
  (let (
      (project (unwrap! (map-get? projects { project-id: project-id })
        ERR-PROJECT-NOT-FOUND
      ))
      (milestone (unwrap!
        (map-get? milestones {
          project-id: project-id,
          milestone-id: milestone-id,
        })
        ERR-MILESTONE-NOT-FOUND
      ))
      (creator (get creator project))
      (amount (get funding-amount milestone))
    )
    ;; Update milestone status
    (map-set milestones {
      project-id: project-id,
      milestone-id: milestone-id,
    }
      (merge milestone { status: MILESTONE-COMPLETED })
    )
    ;; Transfer funds to creator - Use try! to handle the response
    (try! (as-contract (stx-transfer? amount tx-sender creator)))
    
    ;; Check if all milestones are completed
    (if (all-milestones-completed project-id)
      (map-set projects { project-id: project-id }
        (merge project { status: STATUS-COMPLETED })
      )
      true
    )
    
    (ok true)
  )
)


;; Helper to refund all contributions for a cancelled project
(define-private (refund-all-contributions (project-id uint))
  ;; This is a simplified version - in a real contract you would need to 
  ;; iterate through all contributors and refund them
  ;; Since Clarity doesn't have loops, you'd need an off-chain solution to call a refund function
  ;; for each contributor, or implement a withdrawal pattern
  
  ;; Return (ok true) for now, but ensure the error type is defined as uint for the type checker.
  ;; Adding an (if true ...) ensures the type checker sees a potential (err uint) path.
  (if true 
    (ok true)
    (err u999) ;; Placeholder error to define the error type as uint
  )
)

;; Read-only functions
;; Get project details
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

;; Get milestone details
(define-read-only (get-milestone
    (project-id uint)
    (milestone-id uint)
  )
  (map-get? milestones {
    project-id: project-id,
    milestone-id: milestone-id,
  })
)

;; Get contribution amount
(define-read-only (get-contribution
    (project-id uint)
    (contributor principal)
  )
  (default-to { amount: u0 }
    (map-get? contributions {
      project-id: project-id,
      contributor: contributor,
    })
  )
)

;; Get verification votes
(define-read-only (get-vote-counts
    (project-id uint)
    (milestone-id uint)
  )
  (map-get? vote-counts {
    project-id: project-id,
    milestone-id: milestone-id,
  })
)

;; Check if an address is a designated verifier
(define-read-only (is-designated-verifier
    (project-id uint)
    (verifier principal)
  )
  (match (map-get? designated-verifiers {
    project-id: project-id,
    verifier: verifier,
  })
    verifier-info (get active verifier-info)
    false
  )
)

;; Get total projects created
(define-read-only (get-total-projects)
  (var-get project-id-counter)
)

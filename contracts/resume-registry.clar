;; title: resume-registry
;; version:
;; summary:
;; description:
;; Resume Registry Smart Contract
;; A decentralized system for registering and verifying professional resumes on the blockchain
;; Enables job seekers to create tamper-proof work history records and skill verifications

;; Constants for error handling
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-RESUME-NOT-FOUND (err u101))
(define-constant ERR-INVALID-INPUT (err u102))
(define-constant ERR-ALREADY-EXISTS (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-VERIFICATION-FAILED (err u105))
(define-constant ERR-INVALID-STATUS (err u106))
(define-constant ERR-SKILL-NOT-FOUND (err u107))
(define-constant ERR-EXPERIENCE-NOT-FOUND (err u108))

;; Contract owner for administrative functions
(define-constant CONTRACT-OWNER tx-sender)

;; Registration fee in microSTX
(define-constant REGISTRATION-FEE u1000000) ;; 1 STX

;; Maximum string lengths for validation
(define-constant MAX-NAME-LENGTH u100)
(define-constant MAX-TITLE-LENGTH u100)
(define-constant MAX-DESCRIPTION-LENGTH u500)
(define-constant MAX-SKILL-LENGTH u50)
(define-constant MAX-COMPANY-LENGTH u100)

;; Data structures for resume information
(define-map resumes
  { user: principal }
  {
    name: (string-ascii 100),
    title: (string-ascii 100),
    description: (string-ascii 500),
    created-at: uint,
    updated-at: uint,
    verified: bool,
    verification-score: uint,
    total-endorsements: uint
  }
)

;; Work experience records
(define-map work-experience
  { user: principal, experience-id: uint }
  {
    company: (string-ascii 100),
    position: (string-ascii 100),
    description: (string-ascii 500),
    start-date: uint,
    end-date: uint,
    verified: bool,
    verifier: (optional principal)
  }
)

;; Skills and certifications
(define-map skills
  { user: principal, skill-id: uint }
  {
    skill-name: (string-ascii 50),
    proficiency-level: uint, ;; 1-5 scale
    verified: bool,
    endorsements: uint,
    certification-url: (optional (string-ascii 200))
  }
)

;; Skill endorsements tracking
(define-map skill-endorsements
  { endorser: principal, user: principal, skill-id: uint }
  { endorsed: bool, timestamp: uint }
)

;; User counters for generating unique IDs
(define-map user-counters
  { user: principal }
  { experience-count: uint, skill-count: uint }
)

;; Global statistics
(define-data-var total-resumes uint u0)
(define-data-var total-verifications uint u0)

;; Helper function to validate string length
(define-private (validate-string-length (input (string-ascii 500)) (max-length uint))
  (<= (len input) max-length)
)

;; Helper function to get current block height as timestamp
(define-private (get-current-timestamp)
  stacks-block-height
)

;; Helper function to get user counters or initialize them
(define-private (get-or-init-counters (user principal))
  (default-to 
    { experience-count: u0, skill-count: u0 }
    (map-get? user-counters { user: user })
  )
)

;; Register a new resume
(define-public (register-resume 
  (name (string-ascii 100))
  (title (string-ascii 100))
  (description (string-ascii 500))
)
  (let (
    (current-resume (map-get? resumes { user: tx-sender }))
    (current-timestamp (get-current-timestamp))
  )
    (asserts! (is-none current-resume) ERR-ALREADY-EXISTS)
    (asserts! (validate-string-length name MAX-NAME-LENGTH) ERR-INVALID-INPUT)
    (asserts! (validate-string-length title MAX-TITLE-LENGTH) ERR-INVALID-INPUT)
    (asserts! (validate-string-length description MAX-DESCRIPTION-LENGTH) ERR-INVALID-INPUT)
    (asserts! (> (len name) u0) ERR-INVALID-INPUT)
    
    ;; Charge registration fee
    (try! (stx-transfer? REGISTRATION-FEE tx-sender CONTRACT-OWNER))
    
    ;; Create resume record
    (map-set resumes
      { user: tx-sender }
      {
        name: name,
        title: title,
        description: description,
        created-at: current-timestamp,
        updated-at: current-timestamp,
        verified: false,
        verification-score: u0,
        total-endorsements: u0
      }
    )
    
    ;; Initialize user counters
    (map-set user-counters
      { user: tx-sender }
      { experience-count: u0, skill-count: u0 }
    )
    
    ;; Update global statistics
    (var-set total-resumes (+ (var-get total-resumes) u1))
    
    (ok true)
  )
)

;; Update existing resume
(define-public (update-resume
  (name (string-ascii 100))
  (title (string-ascii 100))
  (description (string-ascii 500))
)
  (let (
    (current-resume (unwrap! (map-get? resumes { user: tx-sender }) ERR-RESUME-NOT-FOUND))
    (current-timestamp (get-current-timestamp))
  )
    (asserts! (validate-string-length name MAX-NAME-LENGTH) ERR-INVALID-INPUT)
    (asserts! (validate-string-length title MAX-TITLE-LENGTH) ERR-INVALID-INPUT)
    (asserts! (validate-string-length description MAX-DESCRIPTION-LENGTH) ERR-INVALID-INPUT)
    (asserts! (> (len name) u0) ERR-INVALID-INPUT)
    
    ;; Update resume with new information
    (map-set resumes
      { user: tx-sender }
      (merge current-resume {
        name: name,
        title: title,
        description: description,
        updated-at: current-timestamp
      })
    )
    
    (ok true)
  )
)

;; Add work experience
(define-public (add-work-experience
  (company (string-ascii 100))
  (position (string-ascii 100))
  (description (string-ascii 500))
  (start-date uint)
  (end-date uint)
)
  (let (
    (resume-exists (is-some (map-get? resumes { user: tx-sender })))
    (counters (get-or-init-counters tx-sender))
    (new-experience-id (+ (get experience-count counters) u1))
  )
    (asserts! resume-exists ERR-RESUME-NOT-FOUND)
    (asserts! (validate-string-length company MAX-COMPANY-LENGTH) ERR-INVALID-INPUT)
    (asserts! (validate-string-length position MAX-TITLE-LENGTH) ERR-INVALID-INPUT)
    (asserts! (validate-string-length description MAX-DESCRIPTION-LENGTH) ERR-INVALID-INPUT)
    (asserts! (< start-date end-date) ERR-INVALID-INPUT)
    (asserts! (> (len company) u0) ERR-INVALID-INPUT)
    (asserts! (> (len position) u0) ERR-INVALID-INPUT)
    
    ;; Add work experience record
    (map-set work-experience
      { user: tx-sender, experience-id: new-experience-id }
      {
        company: company,
        position: position,
        description: description,
        start-date: start-date,
        end-date: end-date,
        verified: false,
        verifier: none
      }
    )
    
    ;; Update counters
    (map-set user-counters
      { user: tx-sender }
      (merge counters { experience-count: new-experience-id })
    )
    
    (ok new-experience-id)
  )
)

;; Add skill
(define-public (add-skill
  (skill-name (string-ascii 50))
  (proficiency-level uint)
  (certification-url (optional (string-ascii 200)))
)
  (let (
    (resume-exists (is-some (map-get? resumes { user: tx-sender })))
    (counters (get-or-init-counters tx-sender))
    (new-skill-id (+ (get skill-count counters) u1))
  )
    (asserts! resume-exists ERR-RESUME-NOT-FOUND)
    (asserts! (validate-string-length skill-name MAX-SKILL-LENGTH) ERR-INVALID-INPUT)
    (asserts! (and (>= proficiency-level u1) (<= proficiency-level u5)) ERR-INVALID-INPUT)
    (asserts! (> (len skill-name) u0) ERR-INVALID-INPUT)
    
    ;; Add skill record
    (map-set skills
      { user: tx-sender, skill-id: new-skill-id }
      {
        skill-name: skill-name,
        proficiency-level: proficiency-level,
        verified: false,
        endorsements: u0,
        certification-url: certification-url
      }
    )
    
    ;; Update counters
    (map-set user-counters
      { user: tx-sender }
      (merge counters { skill-count: new-skill-id })
    )
    
    (ok new-skill-id)
  )
)

;; Endorse a skill (by another user)
(define-public (endorse-skill (user principal) (skill-id uint))
  (let (
    (skill-record (unwrap! (map-get? skills { user: user, skill-id: skill-id }) ERR-SKILL-NOT-FOUND))
    (existing-endorsement (map-get? skill-endorsements { endorser: tx-sender, user: user, skill-id: skill-id }))
    (current-timestamp (get-current-timestamp))
  )
    (asserts! (not (is-eq tx-sender user)) ERR-NOT-AUTHORIZED)
    (asserts! (is-none existing-endorsement) ERR-ALREADY-EXISTS)
    
    ;; Record the endorsement
    (map-set skill-endorsements
      { endorser: tx-sender, user: user, skill-id: skill-id }
      { endorsed: true, timestamp: current-timestamp }
    )
    
    ;; Update skill endorsement count
    (map-set skills
      { user: user, skill-id: skill-id }
      (merge skill-record { endorsements: (+ (get endorsements skill-record) u1) })
    )
    
    ;; Update user's total endorsements in resume
    (match (map-get? resumes { user: user })
      resume-data (map-set resumes
                    { user: user }
                    (merge resume-data { total-endorsements: (+ (get total-endorsements resume-data) u1) }))
      false
    )
    
    (ok true)
  )
)

;; Verify resume (admin function)
(define-public (verify-resume (user principal) (verification-score uint))
  (let (
    (resume-data (unwrap! (map-get? resumes { user: user }) ERR-RESUME-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= verification-score u100) ERR-INVALID-INPUT)
    
    ;; Update resume verification
    (map-set resumes
      { user: user }
      (merge resume-data {
        verified: true,
        verification-score: verification-score
      })
    )
    
    ;; Update global verification count
    (var-set total-verifications (+ (var-get total-verifications) u1))
    
    (ok true)
  )
)

;; Get resume information
(define-read-only (get-resume (user principal))
  (map-get? resumes { user: user })
)

;; Get work experience
(define-read-only (get-work-experience (user principal) (experience-id uint))
  (map-get? work-experience { user: user, experience-id: experience-id })
)

;; Get skill information
(define-read-only (get-skill (user principal) (skill-id uint))
  (map-get? skills { user: user, skill-id: skill-id })
)

;; Get user counters
(define-read-only (get-user-counters (user principal))
  (get-or-init-counters user)
)

;; Check if skill is endorsed by specific user
(define-read-only (check-skill-endorsement (endorser principal) (user principal) (skill-id uint))
  (is-some (map-get? skill-endorsements { endorser: endorser, user: user, skill-id: skill-id }))
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-resumes: (var-get total-resumes),
    total-verifications: (var-get total-verifications),
    registration-fee: REGISTRATION-FEE
  }
)

;; Check if resume exists
(define-read-only (resume-exists (user principal))
  (is-some (map-get? resumes { user: user }))
)
;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;


;; title: hiring-contract
;; version:
;; summary:
;; description:
;; Hiring Contract Smart Contract
;; A decentralized system for creating trustless job offers with conditional payments
;; Enables employers to post jobs, manage applications, and automate milestone-based payments

;; Constants for error handling
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-JOB-NOT-FOUND (err u201))
(define-constant ERR-INVALID-INPUT (err u202))
(define-constant ERR-ALREADY-EXISTS (err u203))
(define-constant ERR-INSUFFICIENT-FUNDS (err u204))
(define-constant ERR-INVALID-STATUS (err u205))
(define-constant ERR-APPLICATION-NOT-FOUND (err u206))
(define-constant ERR-MILESTONE-NOT-FOUND (err u207))
(define-constant ERR-INVALID-MILESTONE (err u208))
(define-constant ERR-JOB-NOT-ACTIVE (err u209))
(define-constant ERR-ALREADY-APPLIED (err u210))
(define-constant ERR-NOT-HIRED (err u211))
(define-constant ERR-MILESTONE-COMPLETED (err u212))
(define-constant ERR-DISPUTE-EXISTS (err u213))

;; Job status constants
(define-constant STATUS-DRAFT u0)
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-IN-PROGRESS u2)
(define-constant STATUS-COMPLETED u3)
(define-constant STATUS-CANCELLED u4)
(define-constant STATUS-DISPUTED u5)

;; Application status constants
(define-constant APP-STATUS-PENDING u0)
(define-constant APP-STATUS-ACCEPTED u1)
(define-constant APP-STATUS-REJECTED u2)
(define-constant APP-STATUS-WITHDRAWN u3)

;; Milestone status constants
(define-constant MILESTONE-STATUS-PENDING u0)
(define-constant MILESTONE-STATUS-IN-PROGRESS u1)
(define-constant MILESTONE-STATUS-COMPLETED u2)
(define-constant MILESTONE-STATUS-APPROVED u3)
(define-constant MILESTONE-STATUS-REJECTED u4)

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant PLATFORM-FEE-RATE u250) ;; 2.5% in basis points (250/10000)
(define-constant MAX-TITLE-LENGTH u200)
(define-constant MAX-DESCRIPTION-LENGTH u1000)
(define-constant MAX-REQUIREMENTS-LENGTH u500)
(define-constant MAX-MILESTONES u10)

;; Job posting structure
(define-map jobs
  { job-id: uint }
  {
    employer: principal,
    title: (string-ascii 200),
    description: (string-ascii 1000),
    requirements: (string-ascii 500),
    total-budget: uint,
    escrowed-amount: uint,
    status: uint,
    created-at: uint,
    updated-at: uint,
    hired-candidate: (optional principal),
    total-milestones: uint,
    completed-milestones: uint
  }
)

;; Job applications
(define-map applications
  { job-id: uint, applicant: principal }
  {
    proposal: (string-ascii 500),
    proposed-rate: uint,
    application-date: uint,
    status: uint,
    cover-letter: (string-ascii 1000)
  }
)

;; Milestones for job completion
(define-map milestones
  { job-id: uint, milestone-id: uint }
  {
    title: (string-ascii 200),
    description: (string-ascii 500),
    amount: uint,
    deadline: uint,
    status: uint,
    submitted-at: (optional uint),
    approved-at: (optional uint),
    deliverable-url: (optional (string-ascii 300))
  }
)

;; Payment tracking
(define-map payments
  { job-id: uint, payment-id: uint }
  {
    recipient: principal,
    amount: uint,
    milestone-id: uint,
    payment-date: uint,
    transaction-id: (string-ascii 100)
  }
)

;; Dispute management
(define-map disputes
  { job-id: uint }
  {
    plaintiff: principal,
    defendant: principal,
    reason: (string-ascii 500),
    created-at: uint,
    resolved: bool,
    resolution: (optional (string-ascii 300))
  }
)

;; Global counters
(define-data-var next-job-id uint u1)
(define-data-var next-payment-id uint u1)
(define-data-var total-jobs uint u0)
(define-data-var total-completed-jobs uint u0)
(define-data-var total-platform-fees uint u0)

;; Helper function to validate string length
(define-private (validate-string-length (input (string-ascii 1000)) (max-length uint))
  (<= (len input) max-length)
)

;; Helper function to get current timestamp
(define-private (get-current-timestamp)
  stacks-block-height
)

;; Helper function to calculate platform fee
(define-private (calculate-platform-fee (amount uint))
  (/ (* amount PLATFORM-FEE-RATE) u10000)
)

;; Create a new job posting
(define-public (create-job
  (title (string-ascii 200))
  (description (string-ascii 1000))
  (requirements (string-ascii 500))
  (total-budget uint)
)
  (let (
    (job-id (var-get next-job-id))
    (current-timestamp (get-current-timestamp))
  )
    (asserts! (validate-string-length title MAX-TITLE-LENGTH) ERR-INVALID-INPUT)
    (asserts! (validate-string-length description MAX-DESCRIPTION-LENGTH) ERR-INVALID-INPUT)
    (asserts! (validate-string-length requirements MAX-REQUIREMENTS-LENGTH) ERR-INVALID-INPUT)
    (asserts! (> total-budget u0) ERR-INVALID-INPUT)
    (asserts! (> (len title) u0) ERR-INVALID-INPUT)
    (asserts! (> (len description) u0) ERR-INVALID-INPUT)
    
    ;; Create job record
    (map-set jobs
      { job-id: job-id }
      {
        employer: tx-sender,
        title: title,
        description: description,
        requirements: requirements,
        total-budget: total-budget,
        escrowed-amount: u0,
        status: STATUS-DRAFT,
        created-at: current-timestamp,
        updated-at: current-timestamp,
        hired-candidate: none,
        total-milestones: u0,
        completed-milestones: u0
      }
    )
    
    ;; Update counters
    (var-set next-job-id (+ job-id u1))
    (var-set total-jobs (+ (var-get total-jobs) u1))
    
    (ok job-id)
  )
)

;; Add milestone to a job
(define-public (add-milestone
  (job-id uint)
  (title (string-ascii 200))
  (description (string-ascii 500))
  (amount uint)
  (deadline uint)
)
  (let (
    (job-data (unwrap! (map-get? jobs { job-id: job-id }) ERR-JOB-NOT-FOUND))
    (milestone-id (+ (get total-milestones job-data) u1))
  )
    (asserts! (is-eq (get employer job-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status job-data) STATUS-DRAFT) ERR-INVALID-STATUS)
    (asserts! (< (get total-milestones job-data) MAX-MILESTONES) ERR-INVALID-MILESTONE)
    (asserts! (validate-string-length title MAX-TITLE-LENGTH) ERR-INVALID-INPUT)
    (asserts! (> amount u0) ERR-INVALID-INPUT)
    (asserts! (> deadline (get-current-timestamp)) ERR-INVALID-INPUT)
    (asserts! (> (len title) u0) ERR-INVALID-INPUT)
    
    ;; Create milestone record
    (map-set milestones
      { job-id: job-id, milestone-id: milestone-id }
      {
        title: title,
        description: description,
        amount: amount,
        deadline: deadline,
        status: MILESTONE-STATUS-PENDING,
        submitted-at: none,
        approved-at: none,
        deliverable-url: none
      }
    )
    
    ;; Update job milestone count
    (map-set jobs
      { job-id: job-id }
      (merge job-data { total-milestones: milestone-id })
    )
    
    (ok milestone-id)
  )
)

;; Publish job (make it active for applications)
(define-public (publish-job (job-id uint))
  (let (
    (job-data (unwrap! (map-get? jobs { job-id: job-id }) ERR-JOB-NOT-FOUND))
  )
    (asserts! (is-eq (get employer job-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status job-data) STATUS-DRAFT) ERR-INVALID-STATUS)
    (asserts! (> (get total-milestones job-data) u0) ERR-INVALID-MILESTONE)
    
    ;; Escrow the total budget
    (try! (stx-transfer? (get total-budget job-data) tx-sender (as-contract tx-sender)))
    
    ;; Update job status and escrowed amount
    (map-set jobs
      { job-id: job-id }
      (merge job-data {
        status: STATUS-ACTIVE,
        escrowed-amount: (get total-budget job-data),
        updated-at: (get-current-timestamp)
      })
    )
    
    (ok true)
  )
)

;; Apply for a job
(define-public (apply-for-job
  (job-id uint)
  (proposal (string-ascii 500))
  (proposed-rate uint)
  (cover-letter (string-ascii 1000))
)
  (let (
    (job-data (unwrap! (map-get? jobs { job-id: job-id }) ERR-JOB-NOT-FOUND))
    (existing-application (map-get? applications { job-id: job-id, applicant: tx-sender }))
  )
    (asserts! (is-eq (get status job-data) STATUS-ACTIVE) ERR-JOB-NOT-ACTIVE)
    (asserts! (is-none existing-application) ERR-ALREADY-APPLIED)
    (asserts! (not (is-eq (get employer job-data) tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (> (len proposal) u0) ERR-INVALID-INPUT)
    (asserts! (> proposed-rate u0) ERR-INVALID-INPUT)
    
    ;; Create application record
    (map-set applications
      { job-id: job-id, applicant: tx-sender }
      {
        proposal: proposal,
        proposed-rate: proposed-rate,
        application-date: (get-current-timestamp),
        status: APP-STATUS-PENDING,
        cover-letter: cover-letter
      }
    )
    
    (ok true)
  )
)

;; Accept job application and hire candidate
(define-public (hire-candidate (job-id uint) (candidate principal))
  (let (
    (job-data (unwrap! (map-get? jobs { job-id: job-id }) ERR-JOB-NOT-FOUND))
    (application (unwrap! (map-get? applications { job-id: job-id, applicant: candidate }) ERR-APPLICATION-NOT-FOUND))
  )
    (asserts! (is-eq (get employer job-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status job-data) STATUS-ACTIVE) ERR-INVALID-STATUS)
    (asserts! (is-eq (get status application) APP-STATUS-PENDING) ERR-INVALID-STATUS)
    
    ;; Update job status and hired candidate
    (map-set jobs
      { job-id: job-id }
      (merge job-data {
        status: STATUS-IN-PROGRESS,
        hired-candidate: (some candidate),
        updated-at: (get-current-timestamp)
      })
    )
    
    ;; Update application status
    (map-set applications
      { job-id: job-id, applicant: candidate }
      (merge application { status: APP-STATUS-ACCEPTED })
    )
    
    (ok true)
  )
)

;; Submit milestone deliverable
(define-public (submit-milestone
  (job-id uint)
  (milestone-id uint)
  (deliverable-url (string-ascii 300))
)
  (let (
    (job-data (unwrap! (map-get? jobs { job-id: job-id }) ERR-JOB-NOT-FOUND))
    (milestone-data (unwrap! (map-get? milestones { job-id: job-id, milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
  )
    (asserts! (is-eq (some tx-sender) (get hired-candidate job-data)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status job-data) STATUS-IN-PROGRESS) ERR-INVALID-STATUS)
    (asserts! (is-eq (get status milestone-data) MILESTONE-STATUS-PENDING) ERR-MILESTONE-COMPLETED)
    (asserts! (> (len deliverable-url) u0) ERR-INVALID-INPUT)
    
    ;; Update milestone with submission
    (map-set milestones
      { job-id: job-id, milestone-id: milestone-id }
      (merge milestone-data {
        status: MILESTONE-STATUS-COMPLETED,
        submitted-at: (some (get-current-timestamp)),
        deliverable-url: (some deliverable-url)
      })
    )
    
    (ok true)
  )
)

;; Approve milestone and release payment
(define-public (approve-milestone (job-id uint) (milestone-id uint))
  (let (
    (job-data (unwrap! (map-get? jobs { job-id: job-id }) ERR-JOB-NOT-FOUND))
    (milestone-data (unwrap! (map-get? milestones { job-id: job-id, milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
    (payment-id (var-get next-payment-id))
    (payment-amount (get amount milestone-data))
    (platform-fee (calculate-platform-fee payment-amount))
    (net-payment (- payment-amount platform-fee))
    (candidate (unwrap! (get hired-candidate job-data) ERR-NOT-HIRED))
  )
    (asserts! (is-eq (get employer job-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status milestone-data) MILESTONE-STATUS-COMPLETED) ERR-INVALID-STATUS)
    
    ;; Transfer payment to candidate
    (try! (as-contract (stx-transfer? net-payment tx-sender candidate)))
    
    ;; Transfer platform fee to contract owner
    (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT-OWNER)))
    
    ;; Update milestone status
    (map-set milestones
      { job-id: job-id, milestone-id: milestone-id }
      (merge milestone-data {
        status: MILESTONE-STATUS-APPROVED,
        approved-at: (some (get-current-timestamp))
      })
    )
    
    ;; Record payment
    (map-set payments
      { job-id: job-id, payment-id: payment-id }
      {
        recipient: candidate,
        amount: net-payment,
        milestone-id: milestone-id,
        payment-date: (get-current-timestamp),
        transaction-id: "milestone-payment"
      }
    )
    
    ;; Update job completed milestones
    (let ((updated-job (merge job-data { completed-milestones: (+ (get completed-milestones job-data) u1) })))
      (map-set jobs { job-id: job-id } updated-job)
      
      ;; Check if job is complete
      (if (is-eq (get completed-milestones updated-job) (get total-milestones updated-job))
        (begin
          (map-set jobs { job-id: job-id } (merge updated-job { status: STATUS-COMPLETED }))
          (var-set total-completed-jobs (+ (var-get total-completed-jobs) u1))
        )
        false
      )
    )
    
    ;; Update counters
    (var-set next-payment-id (+ payment-id u1))
    (var-set total-platform-fees (+ (var-get total-platform-fees) platform-fee))
    
    (ok payment-id)
  )
)

;; Create dispute
(define-public (create-dispute (job-id uint) (reason (string-ascii 500)))
  (let (
    (job-data (unwrap! (map-get? jobs { job-id: job-id }) ERR-JOB-NOT-FOUND))
    (existing-dispute (map-get? disputes { job-id: job-id }))
  )
    (asserts! (is-none existing-dispute) ERR-DISPUTE-EXISTS)
    (asserts! (or (is-eq tx-sender (get employer job-data)) 
                  (is-eq (some tx-sender) (get hired-candidate job-data))) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status job-data) STATUS-IN-PROGRESS) ERR-INVALID-STATUS)
    (asserts! (> (len reason) u0) ERR-INVALID-INPUT)
    
    ;; Create dispute record
    (map-set disputes
      { job-id: job-id }
      {
        plaintiff: tx-sender,
        defendant: (if (is-eq tx-sender (get employer job-data))
                      (unwrap! (get hired-candidate job-data) ERR-NOT-HIRED)
                      (get employer job-data)),
        reason: reason,
        created-at: (get-current-timestamp),
        resolved: false,
        resolution: none
      }
    )
    
    ;; Update job status
    (map-set jobs
      { job-id: job-id }
      (merge job-data { status: STATUS-DISPUTED })
    )
    
    (ok true)
  )
)

;; Read-only functions

;; Get job details
(define-read-only (get-job (job-id uint))
  (map-get? jobs { job-id: job-id })
)

;; Get application details
(define-read-only (get-application (job-id uint) (applicant principal))
  (map-get? applications { job-id: job-id, applicant: applicant })
)

;; Get milestone details
(define-read-only (get-milestone (job-id uint) (milestone-id uint))
  (map-get? milestones { job-id: job-id, milestone-id: milestone-id })
)

;; Get payment details
(define-read-only (get-payment (job-id uint) (payment-id uint))
  (map-get? payments { job-id: job-id, payment-id: payment-id })
)

;; Get dispute details
(define-read-only (get-dispute (job-id uint))
  (map-get? disputes { job-id: job-id })
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-jobs: (var-get total-jobs),
    total-completed-jobs: (var-get total-completed-jobs),
    total-platform-fees: (var-get total-platform-fees),
    platform-fee-rate: PLATFORM-FEE-RATE
  }
)

;; Check if user has applied for job
(define-read-only (has-applied (job-id uint) (applicant principal))
  (is-some (map-get? applications { job-id: job-id, applicant: applicant }))
)

;; Get next job ID
(define-read-only (get-next-job-id)
  (var-get next-job-id)
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


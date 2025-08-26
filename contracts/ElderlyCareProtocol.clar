;; ElderlyCare Protocol
;; Decentralized elderly care coordination with family involvement and quality assurance
;; A blockchain-based system for managing elderly care services with transparency and trust

;; Define the contract owner
(define-constant contract-owner tx-sender)

;; Error constants
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-data (err u102))
(define-constant err-care-plan-not-found (err u103))
(define-constant err-invalid-rating (err u104))
(define-constant err-already-exists (err u105))

;; Data structures for elderly care management
(define-map care-plans 
  uint 
  {
    elderly-id: principal,
    family-contact: principal,
    care-provider: principal,
    care-type: (string-ascii 50),
    daily-hours: uint,
    monthly-cost: uint,
    status: (string-ascii 20),
    created-at: uint,
    last-updated: uint
  })

(define-map care-quality-ratings
  uint
  {
    plan-id: uint,
    rating: uint, ;; 1-5 scale
    feedback: (string-ascii 200),
    rated-by: principal,
    rating-date: uint
  })

;; Counters for unique IDs
(define-data-var next-plan-id uint u1)
(define-data-var next-rating-id uint u1)

;; Total statistics
(define-data-var total-care-plans uint u0)
(define-data-var total-ratings uint u0)

;; Function 1: Register a new elderly care plan
;; This function allows family members to register care plans for elderly
(define-public (register-care-plan 
                 (elderly-id principal)
                 (family-contact principal)
                 (care-provider principal)
                 (care-type (string-ascii 50))
                 (daily-hours uint)
                 (monthly-cost uint))
  (let ((plan-id (var-get next-plan-id))
        (current-block-height stacks-block-height))
    (begin
      ;; Validation checks
      (asserts! (is-eq tx-sender family-contact) err-not-authorized)
      (asserts! (> daily-hours u0) err-invalid-data)
      (asserts! (> monthly-cost u0) err-invalid-data)
      (asserts! (> (len care-type) u0) err-invalid-data)
      
      ;; Store the care plan
      (map-set care-plans plan-id
        {
          elderly-id: elderly-id,
          family-contact: family-contact,
          care-provider: care-provider,
          care-type: care-type,
          daily-hours: daily-hours,
          monthly-cost: monthly-cost,
          status: "active",
          created-at: current-block-height,
          last-updated: current-block-height
        })
      
      ;; Update counters
      (var-set next-plan-id (+ plan-id u1))
      (var-set total-care-plans (+ (var-get total-care-plans) u1))
      
      ;; Emit event and return plan ID
      (print {
        event: "care-plan-registered",
        plan-id: plan-id,
        elderly-id: elderly-id,
        family-contact: family-contact,
        care-provider: care-provider
      })
      (ok plan-id))))

;; Function 2: Submit quality rating for care service
;; This function allows family members to rate and provide feedback on care quality
(define-public (submit-care-rating
                 (plan-id uint)
                 (rating uint)
                 (feedback (string-ascii 200)))
  (let ((rating-id (var-get next-rating-id))
        (current-block-height stacks-block-height)
        (care-plan (map-get? care-plans plan-id)))
    (begin
      ;; Check if care plan exists
      (asserts! (is-some care-plan) err-care-plan-not-found)
      
      ;; Validate rating (1-5 scale)
      (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
      
      ;; Check authorization - only family contact can rate
      (asserts! (is-eq tx-sender 
                      (get family-contact (unwrap! care-plan err-care-plan-not-found))) 
                err-not-authorized)
      
      ;; Store the rating
      (map-set care-quality-ratings rating-id
        {
          plan-id: plan-id,
          rating: rating,
          feedback: feedback,
          rated-by: tx-sender,
          rating-date: current-block-height
        })
      
      ;; Update counters
      (var-set next-rating-id (+ rating-id u1))
      (var-set total-ratings (+ (var-get total-ratings) u1))
      
      ;; Emit event
      (print {
        event: "care-rating-submitted",
        rating-id: rating-id,
        plan-id: plan-id,
        rating: rating,
        rated-by: tx-sender
      })
      (ok rating-id))))

;; Read-only function to get care plan details
(define-read-only (get-care-plan (plan-id uint))
  (ok (map-get? care-plans plan-id)))

;; Read-only function to get care rating details
(define-read-only (get-care-rating (rating-id uint))
  (ok (map-get? care-quality-ratings rating-id)))

;; Read-only function to get total statistics
(define-read-only (get-protocol-stats)
  (ok {
    total-care-plans: (var-get total-care-plans),
    total-ratings: (var-get total-ratings),
    next-plan-id: (var-get next-plan-id),
    next-rating-id: (var-get next-rating-id)
  }))
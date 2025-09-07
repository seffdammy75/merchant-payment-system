;; Merchant Payment Processing System
;; A comprehensive smart contract for small business payment processing
;; with transaction verification, chargeback management, and fraud detection

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-already-processed (err u105))
(define-constant err-fraud-detected (err u106))
(define-constant err-chargeback-expired (err u107))

;; Data Variables
(define-data-var next-transaction-id uint u1)
(define-data-var next-merchant-id uint u1)
(define-data-var fraud-threshold uint u1000) ;; Max transaction amount without additional verification
(define-data-var chargeback-window uint u144) ;; Blocks (approximately 24 hours)

;; Data Maps
(define-map merchants uint {
  wallet: principal,
  name: (string-ascii 50),
  active: bool,
  total-processed: uint,
  fraud-score: uint
})

(define-map transactions uint {
  merchant-id: uint,
  customer: principal,
  amount: uint,
  status: (string-ascii 20), ;; "pending", "completed", "disputed", "refunded"
  timestamp: uint,
  description: (string-ascii 100),
  fraud-checked: bool
})

(define-map merchant-balances uint uint)
(define-map chargebacks uint {
  transaction-id: uint,
  reason: (string-ascii 200),
  status: (string-ascii 20), ;; "open", "resolved", "denied"
  created-at: uint
})

(define-map fraud-patterns principal uint) ;; Track suspicious customer behavior

;; Read-only functions
(define-read-only (get-merchant (merchant-id uint))
  (map-get? merchants merchant-id)
)

(define-read-only (get-transaction (tx-id uint))
  (map-get? transactions tx-id)
)

(define-read-only (get-merchant-balance (merchant-id uint))
  (default-to u0 (map-get? merchant-balances merchant-id))
)

(define-read-only (get-chargeback (chargeback-id uint))
  (map-get? chargebacks chargeback-id)
)

(define-read-only (is-fraud-risk (customer principal) (amount uint))
  (let (
    (customer-pattern (default-to u0 (map-get? fraud-patterns customer)))
    (threshold (var-get fraud-threshold))
  )
    (or (> amount threshold) (> customer-pattern u3))
  )
)

;; Private functions
(define-private (update-fraud-score (customer principal))
  (let (
    (current-score (default-to u0 (map-get? fraud-patterns customer)))
  )
    (map-set fraud-patterns customer (+ current-score u1))
  )
)

;; Public functions
(define-public (register-merchant (name (string-ascii 50)))
  (let (
    (merchant-id (var-get next-merchant-id))
  )
    (map-set merchants merchant-id {
      wallet: tx-sender,
      name: name,
      active: true,
      total-processed: u0,
      fraud-score: u0
    })
    (map-set merchant-balances merchant-id u0)
    (var-set next-merchant-id (+ merchant-id u1))
    (ok merchant-id)
  )
)

(define-public (process-payment (merchant-id uint) (amount uint) (description (string-ascii 100)))
  (let (
    (tx-id (var-get next-transaction-id))
    (merchant (unwrap! (map-get? merchants merchant-id) err-not-found))
    (is-fraud (is-fraud-risk tx-sender amount))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (get active merchant) err-unauthorized)
    
    ;; Create transaction record
    (map-set transactions tx-id {
      merchant-id: merchant-id,
      customer: tx-sender,
      amount: amount,
      status: (if is-fraud "pending" "completed"),
      timestamp: stacks-block-height,
      description: description,
      fraud-checked: is-fraud
    })
    
    ;; Update merchant balance if not fraud risk
    (if (not is-fraud)
      (let (
        (current-balance (get-merchant-balance merchant-id))
      )
        (map-set merchant-balances merchant-id (+ current-balance amount))
        ;; Update merchant stats
        (map-set merchants merchant-id 
          (merge merchant { total-processed: (+ (get total-processed merchant) amount) })
        )
      )
      ;; Update fraud patterns for suspicious transactions
      (update-fraud-score tx-sender)
    )
    
    (var-set next-transaction-id (+ tx-id u1))
    (ok { transaction-id: tx-id, fraud-check-required: is-fraud })
  )
)

(define-public (approve-transaction (tx-id uint))
  (let (
    (tx-data (unwrap! (map-get? transactions tx-id) err-not-found))
    (merchant (unwrap! (map-get? merchants (get merchant-id tx-data)) err-not-found))
  )
    (asserts! (is-eq tx-sender (get wallet merchant)) err-unauthorized)
    (asserts! (is-eq (get status tx-data) "pending") err-already-processed)
    
    ;; Update transaction status
    (map-set transactions tx-id (merge tx-data { status: "completed" }))
    
    ;; Update merchant balance
    (let (
      (current-balance (get-merchant-balance (get merchant-id tx-data)))
      (amount (get amount tx-data))
    )
      (map-set merchant-balances (get merchant-id tx-data) (+ current-balance amount))
      ;; Update merchant stats
      (map-set merchants (get merchant-id tx-data)
        (merge merchant { total-processed: (+ (get total-processed merchant) amount) })
      )
    )
    
    (ok true)
  )
)

(define-public (withdraw-funds (merchant-id uint) (amount uint))
  (let (
    (merchant (unwrap! (map-get? merchants merchant-id) err-not-found))
    (current-balance (get-merchant-balance merchant-id))
  )
    (asserts! (is-eq tx-sender (get wallet merchant)) err-unauthorized)
    (asserts! (>= current-balance amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set merchant-balances merchant-id (- current-balance amount))
    ;; In a real implementation, this would transfer STX to the merchant
    (ok amount)
  )
)

(define-public (initiate-chargeback (tx-id uint) (reason (string-ascii 200)))
  (let (
    (tx-data (unwrap! (map-get? transactions tx-id) err-not-found))
    (chargeback-deadline (+ (get timestamp tx-data) (var-get chargeback-window)))
  )
    (asserts! (is-eq tx-sender (get customer tx-data)) err-unauthorized)
    (asserts! (is-eq (get status tx-data) "completed") err-not-found)
    (asserts! (<= stacks-block-height chargeback-deadline) err-chargeback-expired)
    
    ;; Update transaction status
    (map-set transactions tx-id (merge tx-data { status: "disputed" }))
    
    ;; Create chargeback record
    (map-set chargebacks tx-id {
      transaction-id: tx-id,
      reason: reason,
      status: "open",
      created-at: stacks-block-height
    })
    
    (ok true)
  )
)

(define-public (resolve-chargeback (tx-id uint) (approve bool))
  (let (
    (tx-data (unwrap! (map-get? transactions tx-id) err-not-found))
    (merchant (unwrap! (map-get? merchants (get merchant-id tx-data)) err-not-found))
    (chargeback (unwrap! (map-get? chargebacks tx-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get wallet merchant)) err-unauthorized)
    (asserts! (is-eq (get status chargeback) "open") err-already-processed)
    
    (if approve
      (begin
        ;; Approve refund
        (map-set transactions tx-id (merge tx-data { status: "refunded" }))
        (map-set chargebacks tx-id (merge chargeback { status: "resolved" }))
        ;; Deduct from merchant balance
        (let (
          (current-balance (get-merchant-balance (get merchant-id tx-data)))
          (refund-amount (get amount tx-data))
        )
          (map-set merchant-balances (get merchant-id tx-data) 
            (if (>= current-balance refund-amount) 
              (- current-balance refund-amount) 
              u0))
        )
      )
      (begin
        ;; Deny chargeback
        (map-set chargebacks tx-id (merge chargeback { status: "denied" }))
        (map-set transactions tx-id (merge tx-data { status: "completed" }))
      )
    )
    
    (ok approve)
  )
)

;; Admin functions
(define-public (update-fraud-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set fraud-threshold new-threshold)
    (ok true)
  )
)

(define-public (deactivate-merchant (merchant-id uint))
  (let (
    (merchant (unwrap! (map-get? merchants merchant-id) err-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set merchants merchant-id (merge merchant { active: false }))
    (ok true)
  )
)


;; title: payment-processor
;; version:
;; summary:
;; description:

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


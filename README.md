# Loan Smart Contract

## Overview
This smart contract facilitates decentralized lending and borrowing by allowing users to create, fund, and repay loans. The contract ensures secure handling of funds, loan tracking, and repayment calculations.

## Features
- **Loan Creation:** Borrowers can request loans by specifying an amount, interest rate, and duration.
- **Loan Funding:** Lenders can fund loans, activating the loan and transferring funds to the borrower.
- **Loan Repayment:** Borrowers repay the loan amount plus interest within the loan duration.
- **Default Detection:** The contract can determine if a loan has been defaulted based on the repayment deadline.
- **Read-Only Functions:** Retrieve loan details and calculate repayment amounts without modifying state.

## Data Structures

### Constants
- **ERR-INVALID-AMOUNT:** Error for invalid loan amounts.
- **ERR-INVALID-DURATION:** Error for invalid loan duration.
- **ERR-LOAN-ACTIVE:** Error if loan is already active.
- **ERR-UNAUTHORIZED:** Error for unauthorized actions.
- **ERR-INSUFFICIENT-FUNDS:** Error for insufficient funds during funding.
- **ERR-LOAN-NOT-ACTIVE:** Error if loan is not active.
- **ERR-ALREADY-REPAID:** Error if loan is already repaid.

### Data Variables
- **loan-counter:** Tracks the total number of loans created.

### Data Maps
- **loans:** Stores loan details including amount, interest rate, duration, start time, borrower, lender, and status.

## Functions

### Read-Only Functions
- **get-loan(loan-id):** Retrieves loan details by loan ID.
- **calculate-repayment-amount(loan-id):** Computes the total amount a borrower needs to repay including interest.
- **is-loan-defaulted(loan-id):** Checks if a loan has passed its repayment deadline without being repaid.

### Public Functions
- **create-loan(amount, interest-rate, duration):** Creates a new loan request.
- **fund-loan(loan-id):** Allows a lender to fund a loan and activate it.
- **repay-loan(loan-id):** Enables the borrower to repay the loan and close it.

## Security Considerations
- Loans can only be repaid by the borrower.
- Funds are securely transferred using the `stx-transfer?` function.
- The contract prevents duplicate loan funding and repayment.
- Unauthorized users cannot modify loan details.

## Usage
1. **Borrowers** create loan requests specifying loan details.
2. **Lenders** fund loans, activating them and transferring funds to borrowers.
3. **Borrowers** repay the loan with interest before the deadline.
4. **The contract** automatically detects defaults if the loan is not repaid on time.

This contract ensures transparency and security in peer-to-peer lending on the blockchain.
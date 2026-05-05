       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-ACCOUNT-SYSTEM.
       AUTHOR. CLAUDE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-ACCOUNT.
          05 WS-ACCOUNT-NUMBER    PIC 9(6)      VALUE 100001.
          05 WS-ACCOUNT-NAME      PIC A(30)     VALUE "John Doe".
          05 WS-BALANCE           PIC 9(9)V99   VALUE 1000.00.

       01 WS-TRANSACTION.
          05 WS-TRANS-AMOUNT      PIC 9(7)V99   VALUE 0.
          05 WS-TRANS-TYPE        PIC A(10)     VALUE SPACES.

       01 WS-CHOICE              PIC 9         VALUE 0.
       01 WS-CONTINUE            PIC A         VALUE 'Y'.
       01 WS-FORMATTED-BAL       PIC $$$,$$$,$$9.99.
       01 WS-FORMATTED-AMT       PIC $$$,$$$,$$9.99.
       01 WS-TEMP-BALANCE        PIC 9(9)V99   VALUE 0.

       PROCEDURE DIVISION.

       MAIN-PARA.
           PERFORM UNTIL WS-CONTINUE = 'N' OR 'n'
               PERFORM DISPLAY-MENU
               PERFORM GET-CHOICE
               EVALUATE WS-CHOICE
                   WHEN 1
                       PERFORM VIEW-BALANCE
                   WHEN 2
                       PERFORM DEPOSIT
                   WHEN 3
                       PERFORM WITHDRAW
                   WHEN 4
                       PERFORM DISPLAY-ACCOUNT-INFO
                   WHEN 5
                       MOVE 'N' TO WS-CONTINUE
                   WHEN OTHER
                       DISPLAY "Invalid option. Please try again."
               END-EVALUATE
           END-PERFORM
           DISPLAY " "
           DISPLAY "Thank you for banking with COBOL Bank!"
           DISPLAY "Goodbye, " FUNCTION TRIM(WS-ACCOUNT-NAME) "."
           STOP RUN.

       DISPLAY-MENU.
           DISPLAY " "
           DISPLAY "======================================"
           DISPLAY "       COBOL BANK ACCOUNT SYSTEM      "
           DISPLAY "======================================"
           DISPLAY "  Account: " WS-ACCOUNT-NUMBER
           DISPLAY "  Holder:  " FUNCTION TRIM(WS-ACCOUNT-NAME)
           DISPLAY "--------------------------------------"
           DISPLAY "  1. View Balance"
           DISPLAY "  2. Deposit"
           DISPLAY "  3. Withdraw"
           DISPLAY "  4. Account Information"
           DISPLAY "  5. Exit"
           DISPLAY "--------------------------------------"
           DISPLAY "  Enter your choice: " WITH NO ADVANCING.

       GET-CHOICE.
           ACCEPT WS-CHOICE.

       VIEW-BALANCE.
           MOVE WS-BALANCE TO WS-FORMATTED-BAL
           DISPLAY " "
           DISPLAY "======================================"
           DISPLAY "           CURRENT BALANCE            "
           DISPLAY "======================================"
           DISPLAY "  " WS-FORMATTED-BAL
           DISPLAY "======================================".

       DEPOSIT.
           DISPLAY " "
           DISPLAY "Enter deposit amount: " WITH NO ADVANCING
           ACCEPT WS-TRANS-AMOUNT
           IF WS-TRANS-AMOUNT <= 0
               DISPLAY "Error: Deposit amount must be positive."
           ELSE
               ADD WS-TRANS-AMOUNT TO WS-BALANCE
               MOVE WS-TRANS-AMOUNT TO WS-FORMATTED-AMT
               MOVE WS-BALANCE TO WS-FORMATTED-BAL
               DISPLAY " "
               DISPLAY "Deposited:   " WS-FORMATTED-AMT
               DISPLAY "New Balance: " WS-FORMATTED-BAL
           END-IF.

       WITHDRAW.
           DISPLAY " "
           DISPLAY "Enter withdrawal amount: " WITH NO ADVANCING
           ACCEPT WS-TRANS-AMOUNT
           IF WS-TRANS-AMOUNT <= 0
               DISPLAY "Error: Withdrawal amount must be positive."
           ELSE
               IF WS-TRANS-AMOUNT > WS-BALANCE
                   DISPLAY "Error: Insufficient funds."
                   MOVE WS-BALANCE TO WS-FORMATTED-BAL
                   DISPLAY "Available balance: " WS-FORMATTED-BAL
               ELSE
                   SUBTRACT WS-TRANS-AMOUNT FROM WS-BALANCE
                   MOVE WS-TRANS-AMOUNT TO WS-FORMATTED-AMT
                   MOVE WS-BALANCE TO WS-FORMATTED-BAL
                   DISPLAY " "
                   DISPLAY "Withdrawn:   " WS-FORMATTED-AMT
                   DISPLAY "New Balance: " WS-FORMATTED-BAL
               END-IF
           END-IF.

       DISPLAY-ACCOUNT-INFO.
           MOVE WS-BALANCE TO WS-FORMATTED-BAL
           DISPLAY " "
           DISPLAY "======================================"
           DISPLAY "         ACCOUNT INFORMATION          "
           DISPLAY "======================================"
           DISPLAY "  Account Number: " WS-ACCOUNT-NUMBER
           DISPLAY "  Account Holder: "
               FUNCTION TRIM(WS-ACCOUNT-NAME)
           DISPLAY "  Current Balance: " WS-FORMATTED-BAL
           DISPLAY "  Account Type:    Checking"
           DISPLAY "  Bank:            COBOL National Bank"
           DISPLAY "======================================".

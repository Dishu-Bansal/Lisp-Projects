(defun nth-fib (n &optional (a 0) (b 1) (current 2)) ; Here n is the nth-number in fibonacci series we have to find. 'a' is the first number. 'b' is the second number. 'current' represents where we are currently in the list as we already have 0 (a) and 1 (b). we are currently at 3rd position, so current is 2 (starting from 0).
  (if (eq n 0) ;Check if user only wants 1st number (0th).
      (values a) ; We return the first number 'a' as we only want 1st number.
      (if (eq n 1) ; Similar to above for second number
          (values b)
          (if (eq n current) ; We don't want first two, so we go further in series and check if our current position is the nth position we want.
              (values (+ a b)) ; WE are at the nth position we want, So we return the number by adding the last two numbers
              (nth-fib n b (+ a b) (+ current 1)))))) ; we are still not at required position. So we update 'a' and 'b' to next numbers. a = b and b = a + b. Then we get to next position by incrementing 'current.

(defun fib (n &optional (ans ())) ; 'n' is the required parameter. 'ans' contains the final list we will return
  (if (eq n (length ans)) ; We check if the number of items in our final list has become equal to the number of items we need.
      (values ans) ; Yes. Our list has as many numbers as asked. So we return the list.
      (values (fib n (append ans (list (nth-fib (length ans)))))))) ;No. Our list is still short. So we find the next number in series using the above 'nth-fib' series. Then we append that to our final list and repeat the process.

(defun fib-lt (n &optional (ans ())) ; 'n' is the required parameter. 'ans' is the final list we will be returning
  (if (< n (nth-fib (length ans))) ; We check if next number (using above nth-fib function) in the series is greater than the provided number.
      (values ans) ; Yes. the next number is greater. So, we don't need to add it to list. Our list is complete and we can return it.
      (values (fib-lt n (append ans (list (nth-fib (length ans)))))))) ; No, next number is smaller. So we find add (append) it to the list and repeat the process. 

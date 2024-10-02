# printing abbreviated data.frames works

    Code
      print_data.frame(x, rows = 7)
    Output
         X1.10 LETTERS.1.10. stats..rnorm.10.
      1   1    A             -0.6264538      
      2   2    B              0.1836433      
      3   3    C             -0.8356286      
      4   4    D              1.5952808      
      
    Message
      <3 rows hidden>
    Output
                                             
      8   8    H              0.7383247      
      9   9    I              0.5757814      
      10 10    J             -0.3053884      

---

    Code
      print_data.frame(x, rows = 7, cols = 2)
    Output
         X1.10 <1 col hidden> stats..rnorm.10.
      1   1          -        -0.6264538      
      2   2          -         0.1836433      
      3   3          -        -0.8356286      
      4   4          -         1.5952808      
      
    Message
      <3 rows hidden>
    Output
                                              
      8   8          -         0.7383247      
      9   9          -         0.5757814      
      10 10          -        -0.3053884      

---

    Code
      print_data.frame(x, rows = 7, cols = 2, digits = 1)
    Output
         X1.10 <1 col hidden> stats..rnorm.10.
      1   1          -        -0.6            
      2   2          -         0.2            
      3   3          -        -0.8            
      4   4          -         1.6            
      
    Message
      <3 rows hidden>
    Output
                                              
      8   8          -         0.7            
      9   9          -         0.6            
      10 10          -        -0.3            

---

    Code
      print_data.frame(x, rows = 7, cols = 2, digits = 1, row.names = FALSE)
    Output
       X1.10 <1 col hidden> stats..rnorm.10.
        1          -        -0.6            
        2          -         0.2            
        3          -        -0.8            
        4          -         1.6            
      
    Message
      <3 rows hidden>
    Output
                                            
        8          -         0.7            
        9          -         0.6            
       10          -        -0.3            

---

    Code
      print_data.frame(x, rows = 7, cols = 2, digits = 1, col.names = FALSE)
    Output
            <1 col hidden>     
      1   1       -        -0.6
      2   2       -         0.2
      3   3       -        -0.8
      4   4       -         1.6
      
    Message
      <3 rows hidden>
    Output
                               
      8   8       -         0.7
      9   9       -         0.6
      10 10       -        -0.3

---

    Code
      print_data.frame(data.frame())
    Message
      < data.frame with 0 columns and 0 rows >

---

    Code
      print_data.frame(data.frame(x = numeric()))
    Message
      < data.frame with 1 column and 0 rows >


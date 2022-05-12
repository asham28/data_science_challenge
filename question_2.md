
## Question 2

**1. How many orders were shipped by Speedy Express in total?**

```sql
SELECT 
  ShipperID, 
  ShipperName, 
  COUNT(OrderID) 
FROM 
  Shippers 
  LEFT JOIN orders USING (ShipperID);
```
> Answer: Speedy Express ships 54 orders in total.

**2. What is the last name of the employee with the most orders?**

```sql
SELECT 
  COUNT(OrderID), 
  EmployeeID, 
  LastName 
FROM 
  Orders 
  LEFT JOIN Employees USING (EmployeeID) 
GROUP BY 
  EmployeeID 
ORDER BY 
  COUNT(OrderID) DESC;
```

> Answer: The employee’s last name with most orders is Peacock (Employee 4).
  
**3. What product was ordered the most by customers in Germany?**
 
```sql 
SELECT 
  ProductID, 
  ProductName, 
  COUNT(*) as UnitsSold 
FROM 
  OrderDetails 
  LEFT JOIN Orders USING (OrderID) 
  LEFT JOIN Customers USING (CustomerID) 
  LEFT JOIN Products USING (ProductID) 
WHERE 
  country = 'Germany' 
GROUP BY 
  ProductID 
ORDER BY 
  UnitsSold DESC;
```
> Answer: Product with ID 31, Gorgonzola Telino, had the most units sold in Germany.


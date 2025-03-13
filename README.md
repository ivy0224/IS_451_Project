# I S 451 - Business Data Analytics Project
# Hotel Cancellation Analysis
## Objective
Our objective is to use the Hotel Booking Cancellation dataset to analyze the factors influencing customers' decisions to cancel hotel bookings. We aim to train different models and use them to predict whether a future booking will be canceled. By doing so, we hope to identify key factors contributing to cancellations and develop insights to improve hotel revenue management.
## Focus
- Identifying **key predictors** of booking cancellations, such as lead time, average room price, payment method, and booking channels.
- Developing and evaluating different classification models (e.g., **logistic regression**, **decision trees**, **random forest**) to predict whether a booking will be canceled.
- Comparing model performance using confusion matrix, including **accuracy**, **sensitivity**, **specificity**, and **balanced accuracy**, to help determine the best predictive approach in different situations. 
- Developing strategies for the hotel on **revenue optimization**, **cancellation policy decisions**, and **operational efficiency**.
## Visulaization
- **Booking status distribution** (Pie Chart)
  - Among 30k records, most of the bookings are not canceled, so bookings that were canceled should be our focus for modeling
- **Correlation plot** (Bar Chart)
  - Individually, among numeric values, "lead time", "special requests", "year", and "average price", have the highest correlation with booking status
- **Lead time vs. booking status** (Boxplot)
  - The median, Q1, and Q3 of lead time for bookings that were canceled are all much longer than those that were not canceled
- **Average Price distribution by booking status** (Bar Chart)
  - A nearly consistent rise in the percentage of cancellations in each category as the price goes higher
- **Special requests by booking status** (Line Chart)
  - The number of special requests is always higher for bookings that are not canceled compared to those that are canceled
- **Market segment type by booking status** (Bar Chart)
  - Online bookings are the majority but also have the highest cancellations. Corporate bookings seem to have the lowest cancellation rate
## Modeling (RStudio)
- **Goal**: Predict whether a future booking will be canceled or not
- **Target(Y)**: "booking status"
- **Predictors(X)**: 16 potential variables such as "lead time", "special requests", etc.
- **Models**
  -  ***Logistic Regression***
  -  ***Default Decision Tree***
  -  ***Decision Tree with Control***
  -  ***Random Forest Tree Ensemble***
## Key Takeaways and Insights
- **Business Implication**
  - **Special Requests**
    - The higher the special requests, the less cancellation possibility
    - Invest in customer relationship management (CRM) and offer loyalty programs
      - Retention rates may increase
      - Deciding factor over competitors
  - **Months/Seasons**
    - Summer seasons are highest in cancellation
    - Based on seasons, hotels can reduce revenue loss and optimize occupancy by overbooking strategically 
      - Minimizes empty rooms at a certain time
  - **Market Segment Type**
    - Pricing Model Adjustment
      - Charge corporate segments higher as they are more inelastic
    - Marketing Strategies
      - Focus on target audience: low cancellation segments such as "Corporate" and "Aviation"
- **Customer Implication**
  - Avoid uncertainty in trip planning
  - If booking 1+ month in advance, look for flexible cancellation policies
## Data
- **Source**: https://www.kaggle.com/datasets/youssefaboelwafa/hotel-booking-cancellation-prediction/data
- **Description**: The dataset contains booking data for hotels, including information on customer demographics, booking dates, and whether the booking was canceled.
  - ***Columns***: 
    - `Booking_ID`: Unique identifier for each booking
    - `number of adults`: Number of adults included in the booking
    - `number of children`: Number of children included in the booking
    - `number of weekend nights`: Number of weekend nights included in the booking
    - `number of week nights`: Number of week nights included in the booking
    - `type of meal`: Type of meal included in the booking
    - `car parking space`: Indicates whether a car parking space was requested or included in the booking
    - `room type`: Type of room booked
    - `lead time`: Number of days between the booking date and the arrival date
    - `market segment type`: Type of market segment associated with the booking
    - `repeated`: Indicates whether the booking is a repeat booking
    - `P-C`: Number of previous bookings that were canceled by the customer prior to the current booking
    - `P-not-C`: Number of previous bookings not canceled by the customer prior to the current booking
    - `average price`: Average price associated with the booking
    - `special requests`: Number of special requests made by the guest
    - `date of reservation`: Date of the reservation
    - `booking status`: Status of the booking ( canceled or not canceled)

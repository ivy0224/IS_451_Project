# I S 451 - Business Data Analytics Project
## Objective
Our objective is to use the Hotel Booking Cancellation dataset to analyze the factors influencing customers' decisions to cancel hotel bookings. We aim to train different models and use them to predict whether a future booking will be canceled. By doing so, we hope to identify key factors contributing to cancellations and develop insights to improve hotel revenue management.
## Focus
- Identifying **key predictors** of booking cancellations, such as lead time, average room price, payment method, and booking channels.
- Developing and evaluating different classification models (e.g., **logistic regression**, **decision trees**, **random forest**) to predict whether a booking will be canceled.
- Comparing model performance using confusion matrix, including **accuracy**, **sensitivity**, **specificity**, and **balanced accuracy**, to help determine the best predictive approach in different situations. 
- Developing strategies for the hotel on **revenue optimization**, **cancellation policy decisions**, and **operational efficiency**.
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

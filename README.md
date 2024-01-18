RStudio files for project works 
ProfComms.r - Video games sales dataset analysis - 40 years of video game data analyzed 

Some characteristics of the dataset used: 
<img width="864" alt="Screenshot 2024-01-18 at 6 38 21 PM" src="https://github.com/saheelchowdhury/rfiles/assets/153671296/6edb633a-9e9e-459d-bbab-8e8e48abc016">

How the filteration process was approached to categorize data according to genre and publisher

#top_publishers <- total_sales[order(total_sales$total_profit, decreasing = TRUE),][1:10,]
<img width="870" alt="Screenshot 2024-01-18 at 6 38 50 PM" src="https://github.com/saheelchowdhury/rfiles/assets/153671296/d06d865f-3da6-4eb1-b528-52e1222a8c3e">

Highest selling publishers by NA & EU region, year range 2010-2020

#code : games_sales <- games_sales[games_sales$year >= 2010 & games_sales$year <= 2020,]

<img width="869" alt="Screenshot 2024-01-18 at 6 39 35 PM" src="https://github.com/saheelchowdhury/rfiles/assets/153671296/f2b4293b-69f3-4339-bb5e-3d006a2a4787">

Game publishers with multiple successful genre were separated : 
<img width="857" alt="Screenshot 2024-01-18 at 6 40 13 PM" src="https://github.com/saheelchowdhury/rfiles/assets/153671296/5f815541-a56f-4d16-8d96-67bd0b39151e">

Finding out highest selling games by publishers - EA, Ubisoft, Rockstar - 2010 to 2020
EA
<img width="838" alt="Screenshot 2024-01-18 at 6 41 07 PM" src="https://github.com/saheelchowdhury/rfiles/assets/153671296/74d09fa5-bd2b-40af-8844-1a434b60e39a">
Ubisoft
<img width="832" alt="Screenshot 2024-01-18 at 6 49 55 PM" src="https://github.com/saheelchowdhury/Video-games-sales-dataset-analysis/assets/153671296/d3f226a4-aa72-4baa-872c-a1e886c68f63">
Rockstar
<img width="835" alt="Screenshot 2024-01-18 at 6 50 06 PM" src="https://github.com/saheelchowdhury/Video-games-sales-dataset-analysis/assets/153671296/975e8394-667e-4f15-bfc7-7f5609d999f9">

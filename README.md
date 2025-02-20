# Life Insurance Portfolio Dashboard

The **Life Insurance Portfolio Dashboard** is an interactive web application designed to simulate and analyze life insurance portfolios. Built using R, Quarto, and Shiny, this tool allows users to explore various actuarial concepts by adjusting multiple input parameters and observing their effects on fund performance over time.

## Features

-   **Simulation vs. Theoretical Fund Performance**: Compare simulated fund trajectories with theoretical models to understand deviations and assess risks.

-   **Ruin Probability & Expected Final Fund Value**: Calculate the likelihood of fund depletion and project expected final values under different scenarios.

-   **Downloadable Results**: Export simulation results and data tables in multiple formats for further analysis or reporting.

## Target Audience

This dashboard is tailored for actuarial students, professionals, and anyone interested in life actuarial mathematics. It offers an intuitive platform to visualize how different parameters influence a fund's performance over time.

## Data Sources

The application utilizes mortality tables from reputable sources to ensure accurate simulations:

-   **Human Life-Table Database (HLD)**: A comprehensive collection of life tables for various populations and time periods, primarily produced by national statistical offices.

    [lifetable.de](https://www.lifetable.de/?utm_source=chatgpt.com)

-   **Society of Actuaries (SOA)**: Provides a wide range of mortality and other rate tables based on extensive actuarial research.

    [mort.soa.org](https://mort.soa.org/?utm_source=chatgpt.com)

## Installation

1.  **Clone the Repository**: You can do that from the terminal of Rstudio by running:

    ```         
    git clone https://github.com/erikdeluca/Life-Insurance-Portfolio-Dashboard.git
    ```

2.  **Install Required Packages**: Ensure you have R and the necessary packages installed. You can install the required packages using:

    ```         
    install.packages(c("shiny", "tidyverse", "DT", "shinyWidgets", "shinycssloaders"))
    ```

3.  **Run the Application**:

    ```         
    shiny::runApp("Life-Insurance-Portfolio-Dashboard")
    ```

## Contributing

This project is in its beta version. Contributions, suggestions, and feedback are welcome. If you encounter issues or have ideas for improvements, please open an issue or submit a pull request.

## Acknowledgments

Special thanks to:

-   **Rafael Moreno Ruiz**: For his guidance in actuarial life mathematics.

-   **Ermanno Pitacco**: For introducing me to actuarial science.

-   **Anna Rita Bacinello and Pietro Millossovich**: For their mentorship in transforming theory into practical R coding skills.

Their contributions to actuarial science and education have been invaluable.

## Disclaimer

This dashboard is intended for educational and informational purposes only. It does not constitute financial or investment advice. Users should consult with qualified professionals for advice tailored to their specific circumstances. The creators assume no responsibility for decisions made based on the content of this application.

## License

This project is licensed under the AGPL-3.0 License. See the [LICENSE](LICENSE.md) file for details.

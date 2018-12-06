# ExpAcc: Explore the Accrual Landscape

This is the code repository for the paper [Gassen (2018), Exploring the Accrual Landscape by Open Science](https://www.wiwi.hu-berlin.de/rewe/research/expacc_paper.pdf).

In this project, I reproduce and extend the findings of [Bushman, Lerman and Zhang (JAR, 2016): The Changing Landscape of Accrual Accounting](http://onlinelibrary.wiley.com/doi/10.1111/1475-679X.12100/abstract).

You will need WRDS access to Compustat (North America and Global) to reproduce the analysis. In addition you need either to have R and RStudio installed or access to docker (see below). Please refer to the Appendix B in the paper for additional guidance on how to reproduce the analysis.

If you have access to a [docker installation](https://www.docker.com), the most straight-forward way is to use the docker container that is also contained in this repository.

1. Check the Docker settings to make sure that you have at least 2 CPUs, 4 GB of memory and 1 GB of swap space available for Docker

2. Issue the following command to pull the container from Docker Hub and run it in your environment:\
`docker run -d -p 8787:8787 -e PASSWORD=yourpass --name expacc joegassen/expacc`

3. Open your browser and point it to: http://localhost:8787

4. Logon with username "rstudio" und the password you selected in step 2

5. Select all the code in the top-left source window of RStudio (Ctrl-A)

6. Run the code (Ctrl-Enter)

7. When asked for it, enter your WRDS user name and WRDS password (will not be stored)

8. Wait and explore

Please get in touch if you encounter something odd and/or interesting. Open an issue on Github or send an email to gassen@wiwi.hu-berlin.de. Thanks!

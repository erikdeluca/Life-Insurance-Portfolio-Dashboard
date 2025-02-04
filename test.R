library(lifecontingencies)
library(tidyverse)
library(ggplot2)

lifetable <- data(package = "lifecontingencies")$results[, "Item"]
omega <- 110
numPremiIniziale <- 1

#' Gestione Portafoglio
#' Dato un portafoglio, di rischi omogenei tra loro, la funzione calcola rendimento e andamento del fondo e i decessi 
#'
#' @param numeroAssicurati 
#' @param eta età degli assicurati
#' @param rata importo della rata annuale
#' @param fondoInizio importo iniziale del fondo
#' @param numeroPremi numero di premi che gli assicurati pagheranno
#' @param omega età massima raggiungibile
#' @param differimento rendita differita o immediata
#' @param temporanea rendita vitalizia o temporanea
#' @param anniCopertura anni di coperrtura nel caso di rendita temporanea
#' @param rateGarantiteDurata numero di rate garantite
#' @param rendimentoFondoAnnuo tasso di rendimento del fondo 
#' @param tassoAleatorio tasso fisso o aleatorio
#' @param tavolaMortalita tavola di mortalità con cui calcolare il premio
#' @param tassoTecnico tasso usato per calcolare il premio
#' @param tavolaPeriodo tavola utilizzata per simulare i decessi all'interno del portafoglio
#'
#' @return
#' La funzione ritorna l'andamento e il rendimento del fondo, i decessi e il premio che ciascun assicurato dovrà pagare
#' 
#' @export
#'
#' @examples
#' 

gestionePortafoglio = function(#input
  numeroAssicurati = 1000,
  eta = 20,
  rata = 1000,
  fondoInizio = 0,
  numeroPremi = 15,
  omega = 110,
  differimento = 25,
  temporanea = FALSE,
  # temporanea o  vita intera
  anniCopertura = 35,
  rateGarantiteDurata = 5,
  rendimentoFondoAnnuo = 0.02,
  # tasso finanziario
  tassoAleatorio = TRUE,
  mortalitaAleatoria = TRUE,
  tavolaMortalita = lifecontingencies::demoIta$SIM02,
  #tavola utilizzata per la base tecnica
  tassoTecnico = 0.02,
  #tasso utilizzato per la base tecnica
  tavolaPeriodo = lifecontingencies::demoIta$SIM02) {
  # tavola utilizzata per calcolare i morti nel portafoglio
  # vengono inizializzati gli output
  andamentoFondo = NULL
  rendimentoFondo = NULL
  decessi = NULL
  # Fissiamo gli anni di copertura nel caso di una vitalizia
  if (!temporanea)
  {
    anniCopertura = omega - eta
  }
  # vettore anni
  anni = (eta+1):(anniCopertura + eta)
  
  calcoloVettoreTasso = function()
  {
    # DA FARE: random walk
    #il tasso si distribuisce come una normale
    if(tassoAleatorio)
      rnorm(anniCopertura, mean = rendimentoFondoAnnuo, sd = 0.01)
    else rep(rendimentoFondoAnnuo, anniCopertura)
    # accettiamo la possibilità di deflazione nel caso del aleatorio
  }
  
  tassoFinanziario = calcoloVettoreTasso()
  
  # calcola quante persone muoiono nel fondo
  calcoloDecessi = function()
  {
    died = NULL
    for (i in eta:(anniCopertura + eta))
    {
      sopravissuti = numeroAssicurati - sum(died)
      #mu = probabilità di decesso nell'anno i condizionatamente che siano in vita all'anno i
      mu =  (tavolaPeriodo[i + 1] - ifelse(i > (omega - 2), 0, tavolaPeriodo[i +
                                                                               2])) / ifelse(i > (omega - 1), 1, tavolaPeriodo[eta + 1])
      #genera i morti da una Poisson con media quelli che in linea teorica dovrebbero morire
      mortiCasuali = if_else(
        mortalitaAleatoria,
        sum(rpois(numeroAssicurati, mu)),
        round(numeroAssicurati * mu)
      )# se volessimo fare un modello deterministico
      #arrotonda agli interi il numero di morti e nel caso sia > dei sopravissuti, li uccide tutti
      died = c(
        died, 
        ifelse(mortiCasuali > sopravissuti, sopravissuti, mortiCasuali)
      )
    }
    # se stiamo valutando fino a omega ci potrebbe essere un grande gruppo di persone che non muoiono prima a causa del processo di Poisson.
    # In questo caso le alloco in modo casuale nel vettore dei morti con pesi le frequenze di decessi
    if (anniCopertura + eta == omega)
    {
      last_died = sample(
        x = 1:(anniCopertura - 1),
        size = died[anniCopertura + 1],
        prob = (died[1:(anniCopertura-1)] / sum(died[1:(anniCopertura-1)])),
        replace = T
      ) |> 
        table() |> 
        as.data.frame()
      died[last_died$Var1] = died[last_died$Var1] + last_died$Freq
      died[anniCopertura + 1] = 0
    }
    return(died)
  }
  
  # calcola gli hPx tramite l_(x+h) / l_x
  hPx = function(h, x)
  {
    # il +1 è per compensare che in R i vettori partono da 1 mentre l'età parte da zero
    # ifelse serve a evitare il problema dell'età limite di andare fuori dal vettore
    return(ifelse(h + x > omega - 1, 0, tavolaMortalita[h + x + 1]) / tavolaMortalita[x + 1])
  }
  
  # assicurati vivi
  hAV = function(h)
    # indica al tempo t il numero di assicurati sopravissuti
  {
    return(numeroAssicurati - ifelse(h > 0, sum(decessi[1:h]), 0))
  }
  
  
  # PREMIO ----------------------------------------------
  premio = function()
  {
    # axn(
    #   new("actuarialtable", interest = tassoTecnico, x = demoIta$X, lx = demoIta$SIM02, name = "SIM02"),
    #     x = eta,
    #     i = tassoTecnico,
    #     m = differimento,
    #     n = anniCopertura,
    #   k = 1, #pagamenti annuali
    #   
    # )
    # if (rateGarantiteDurata > 0)
    # {
    #   p = rata * (sum((1 + tassoTecnico) ** -c((differimento + 1):(differimento + 1 + rateGarantiteDurata)
    #   )) + sum((1 + tassoTecnico) ** -c((differimento + 1 + rateGarantiteDurata):anniCopertura
    #   ) * hPx(c((differimento + 2 + rateGarantiteDurata):anniCopertura
    #   ), eta)))
    # } else
    # {
    #   p = rata * sum((1 + tassoTecnico) ** -(c((differimento + 1):anniCopertura)) * 
    #                    hPx(c((differimento + 1):anniCopertura), eta))
    # }
    # 
    # if (numeroPremi == 1)
    # {
    #   return(p)
    # } else
    # {
    #   return(p / sum((1 + tassoTecnico) ** -c(0:(numeroPremi - 1)) * hPx(c(
    #     0:(numeroPremi - 1)
    #   ), eta))) # parte da zero perché la prima la pagano tutti
    # }
    
    return(-1) # nel caso l'utente inserisca rate garantite negative. Sì potrebbe mettere un try and catch
  }
  
  # INIZIO ANDAMENTO E RENDIMENTO FONDO ----------------------------------------------
  
  # calcolo dei vari decessi
  decessi = calcoloDecessi()
  
  # hist(map_dbl(1:1E3, ~last(calcoloDecessi())))
  
  #Utilizzo della forumula ricorsiva
  # incasso dei premi e differimento
  if (differimento > 1) {
    for (t in 1:differimento)
    {
      andamentoFondo = c(andamentoFondo,
                         (
                           ifelse(t > numeroPremi, 0, hAV(t) * premio()) +
                             ifelse(t > 1, andamentoFondo[t - 1], fondoInizio)
                         ) * (1 + tassoFinanziario[t]))
    }
  }
  
  # calcolo del fondo dall'inizio del pagamento delle rate
  for (t in (differimento +1):(anniCopertura))
  {
    # le rate vanno pagate alla fine dell'anno, quindi devono essere capitalizzate anch'esse
    andamentoFondo = c(andamentoFondo, 
                       ifelse(
                         t > 1,
                         andamentoFondo[t - 1],
                         fondoInizio + premio() * numeroAssicurati
                       ) * (1 + tassoFinanziario[t]) -
                         (
                           rata * ifelse((t - differimento) > rateGarantiteDurata,
                                         hAV(t + 1),
                                         numeroAssicurati
                           )
                         )
    )
  }
  
  
  
  rendimentoFondo = andamentoFondo - c(fondoInizio,andamentoFondo[1:(length(andamentoFondo) - 1)])
  
  
  
  #Creazione dell'oggetto che la funzione dovrà tornare
  # print(c(length(anni),length(andamentoFondo),length(c(replicate(numeroPremi,premio()),
  #                            rep(0,anniCopertura-numeroPremi)))))
  Output = tibble(anni,
                  andamentoFondo,
                  rendimentoFondo,
                  decessi = decessi[2:length(decessi)],
                  premio = c(replicate(numeroPremi, premio()),
                             rep(0, differimento-numeroPremi),
                             -rep(rata, anniCopertura-differimento)),
                  tasso = tassoFinanziario)
  
  return(Output)
  
}


gestionePortafoglio(
  rata = 1,
  tassoAleatorio = FALSE,
  mortalitaAleatoria = FALSE,
  fondoInizio = 0,
  differimento = 2,
  numeroPremi = 1,
  # omega = 118,
  rateGarantiteDurata = 0,
  anniCopertura = 10,
  tassoTecnico = .02,
  eta = 20
) |> pull(premio) |> max()
  # ) |> 
  # slice_max(anni)

axn(new("actuarialtable", interest = .02, x = demoIta$X, lx = demoIta$SIM02, name = "SIM02"),
    x = 20,
    i = .02,
    m = 3,
    n = 8,
    # type = "EV"
    type = "ST"
    )
premio()

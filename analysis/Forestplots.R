library(metafor)

## -------------------------------------------------
## 1. Fit multilevel model
## -------------------------------------------------
res.mv <- rma.mv(
  z_value,
  z_variance,
  random = ~ as.factor(sr_no) | as.factor(study_ID),
  data   = data_usage,
  method = "REML"
)

summary(res.mv, digits = 2)

## -------------------------------------------------
## 2. Prepare plotting data
## -------------------------------------------------
data_plot <- data_usage

data_plot$slab <- as.character(data_plot$study_ID)

data_plot$intervention <- factor(
  data_plot$intervention,
  levels = c("Label", "Information", "Rebate",
             "Subsidy", "Command & Control", "Loan")
)

## Order for readability
data_plot <- data_plot[order(data_plot$intervention), ]

k <- nrow(data_plot)

## DEFINE ROWS EXPLICITLY (TOP ??? BOTTOM)
rows <- k:1

## -------------------------------------------------
## 3. Color palette
## -------------------------------------------------
palette <- c(
  "Label" = "#1f78b4",
  "Information" = "#e31a1c",
  "Rebate" = "#33a02c",
  "Subsidy" = "#6a3d9a",
  "Command & Control" = "#ff7f00",
  "Loan" = "#b15928"
)

cols <- palette[data_plot$intervention]

## -------------------------------------------------
## 4. Base forest plot (NO POINTS)
## -------------------------------------------------

pdf(
  file = "forest_plot_multilevel.pdf",
  width = 10,   # inches (good for journals)
  height = 12,  # tall enough for many studies
  family = "Helvetica"
)

layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1))

par(mar = c(4, 1.5, 1, 1))

forest(
  x       = data_plot$z_value,
  vi      = data_plot$z_variance,
  slab    = data_plot$slab,
  rows    = rows,
  xlab    = "Effect Size",
  refline = 0,
  cex     = 0.85,
  xlim    = c(-0.6, 0.35),
  alim    = c(-0.4, 0.3),
  at      = seq(-0.4, 0.3, by = 0.1),
  efac    = 0,
  annotate = FALSE
)

for (i in seq_len(k)) {
  yi  <- data_plot$z_value[i]
  sei <- sqrt(data_plot$z_variance[i])

  arrows(yi - 1.96*sei, rows[i],
         yi + 1.96*sei, rows[i],
         angle = 90, code = 3, length = 0.04,
         col = cols[i], lwd = 1.2)

  points(yi, rows[i], pch = 15, col = cols[i], cex = 0.9)
}

addpoly(
  res.mv,
  row = -1,
  cex = 1.1,
  col = "black",
  mlab = "Multilevel random-effects model"
)


## -------------------------------------------------
## 5. Overlay colored CIs and points
## -------------------------------------------------
for (i in seq_len(k)) {
  
  yi  <- data_plot$z_value[i]
  sei <- sqrt(data_plot$z_variance[i])
  
  ci.low  <- yi - 1.96 * sei
  ci.high <- yi + 1.96 * sei
  
  arrows(
    ci.low, rows[i],
    ci.high, rows[i],
    angle = 90, code = 3,
    length = 0.04,
    col = cols[i],
    lwd = 1.2
  )
  
  points(
    yi, rows[i],
    pch = 15,
    col = cols[i],
    cex = 0.9
  )
}

## -------------------------------------------------
## 6. Add multilevel RE model polygon
## -------------------------------------------------
addpoly(
  res.mv,
  row = -1,
  cex = 1.1,
  col = "black",
  mlab = "Multilevel random-effects model"
)

## -------------------------------------------------
## Legend panel (FIXED)
## -------------------------------------------------
par(mar = c(4, 1.5, 1, 1))  # <- key fix
plot.new()

legend(
  "center",
  legend = names(palette),
  col = palette,
  pch = 15,
  bty = "n",
  cex = 0.9,
  title = "Intervention"
)


dev.off()



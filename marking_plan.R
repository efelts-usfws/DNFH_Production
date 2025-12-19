
library(tidyverse)
library(readxl)


library(tidyverse)
library(readxl)

current_ry <- 2025

sst_ponding.dat <- read_excel("data/ponding_data.xlsx", sheet = "SST") |>
  filter(release_year == current_ry)

range <- sst_ponding.dat |> 
  summarize(min_fish=min(total),
            max_fish=max(total))


# how many ponds to select by group
sample_size <- tibble(
  group = c("Direct", "Red House", "Clear Creek"),
  n_selected = c(9L, 2L, 2L)
)

total_tags <- 32900L          # total CWTs
smp_total  <- 1500L           # SMP tags (all in Direct)
regular_total <- total_tags - smp_total  # 31,400 (must be multiple of 100)

# --- GROUP-LEVEL ALLOCATION (rounded to 100s, preserving sum) ---

allocation_groups <- sst_ponding.dat |>
  filter(cwt == FALSE) |>
  mutate(by_total = sum(total)) |>
  group_by(group) |>
  summarise(
    by_total    = first(by_total),
    group_total = sum(total),
    .groups = "drop"
  ) |>
  mutate(
    group_prop    = group_total / by_total,
    tag_target    = group_prop * total_tags,           # proportional total per group
    smp_tags      = if_else(group == "Direct", smp_total, 0L),
    regular_target = tag_target - smp_tags             # target regular tags per group
  ) |>
  # Work in "hundreds"
  mutate(
    reg_h_target = regular_target / 100,
    reg_h_base   = floor(reg_h_target),
    reg_h_frac   = reg_h_target - reg_h_base
  ) |>
  arrange(desc(reg_h_frac)) |>
  mutate(
    base_sum_h   = sum(reg_h_base),                    # scalar
    remainder_h  = (regular_total / 100L) - base_sum_h, # how many extra hundreds to distribute
    bonus_h      = as.integer(row_number() <= remainder_h),
    reg_h        = reg_h_base + bonus_h,
    regular_tags = reg_h * 100L
  ) |>
  select(group, smp_tags, regular_tags)

# (Optional) sanity check: sums
stopifnot(sum(allocation_groups$regular_tags) == regular_total)
stopifnot(sum(allocation_groups$smp_tags) == smp_total)

# --- SELECT PONDS PER GROUP (your logic, unchanged) ---

mark_selection <- sst_ponding.dat |>
  inner_join(sample_size, by = "group") |>
  group_by(group) |>
  mutate(.rand = runif(n())) |>
  arrange(.rand, .by_group = TRUE) |>
  filter(row_number() <= first(n_selected)) |>
  ungroup() |>
  select(-.rand, -n_selected) |>
  left_join(allocation_groups, by = "group") |>
  left_join(sample_size, by = "group")

# --- PER-POND REGULAR TAGS (rounded to 100s, preserving group sum) ---

mark_selection <- mark_selection |>
  group_by(group) |>
  mutate(
    reg_h_group      = regular_tags / 100L,
    base_h_per_pond  = reg_h_group %/% n_selected,     # even hundreds per pond
    extras_h         = reg_h_group %% n_selected,      # leftover hundreds
    add_h            = as.integer(row_number() <= extras_h), # give +100 to first 'extras_h' ponds
    pond_regular_tags = (base_h_per_pond + add_h) * 100L
  ) |>
  ungroup()

# --- SMP distribution: 5 ponds in Direct get 300 each (unchanged logic) ---

set.seed(42)  # reproducible SMP pond selection (optional)
smp_selection <- mark_selection |>
  filter(group == "Direct") |>
  slice_sample(n = 5) |>
  select(burrows_pond) |>
  mutate(SMP = TRUE)

final <- mark_selection |>
  left_join(smp_selection, by = "burrows_pond") |>
  mutate(
    smp_tag_number = if_else(SMP, 300L, 0L),
    pond_total_tags = pond_regular_tags + smp_tag_number
  ) |>
  select(-SMP)

# --- VALIDATION ---

# 1) Every pond has a multiple of 100
stopifnot(all(final$pond_regular_tags %% 100L == 0L))
stopifnot(all(final$smp_tag_number %% 100L == 0L))
stopifnot(all(final$pond_total_tags %% 100L == 0L))

# 2) Totals match 32,900 (and SMP = 1,500)
stopifnot(sum(final$pond_total_tags, na.rm = TRUE) == total_tags)
stopifnot(sum(final$smp_tag_number, na.rm = TRUE) == smp_total)

# 3) (Optional) sanity by group
summary_by_group <- final |>
  group_by(group) |>
  summarise(
    n_ponds = n(),
    regular_sum = sum(pond_regular_tags),
    smp_sum     = sum(smp_tag_number),
    total_sum   = sum(pond_total_tags),
    .groups     = "drop"
  )
print(summary_by_group)












# set the current release year to produce
# plan for

current_ry <- 2025

sst_ponding.dat <- read_excel("data/ponding_data.xlsx",
                              sheet="SST") |> 
  filter(release_year==current_ry)


# first set number to select by release group


sample_size <- tibble(
  group = c("Direct", "Red House", "Clear Creek"),
  n_selected    = c(9L,2L, 2L)
)


# allocate to groups proportionally then
# figure out how many per pond based on number of 
# selected ponds

allocation.summary <- sst_ponding.dat |> 
  filter(cwt==F) |> 
  mutate(by_total=sum(total)) |> 
  group_by(group) |> 
  summarize(by_total=first(by_total),
           group_total=sum(total),
         group_prop=group_total/by_total,
         tag_number=group_prop*32900) |> 
  mutate(smp_tags=ifelse(group=="Direct",1500,0),
         regular_tags=tag_number-smp_tags) |> 
  left_join(sample_size,by="group") |> 
  mutate(pond_tag_number=regular_tags/n_selected) |> 
  select(group,pond_tag_number)



mark_selection <- sst_ponding.dat |> 
  inner_join(sample_size,by="group") |> 
  group_by(group) |> 
  mutate(.rand=runif(n())) |> 
  arrange(.rand,.by_group = TRUE) |> 
  filter(row_number()<=first(n_selected)) |> 
  ungroup() |> 
  select(-.rand,-n_selected) |> 
  left_join(allocation.summary,by="group")

# now get smp selection from the already selected
# direct release groups

smp_selection <- mark_selection |> 
  filter(group=="Direct") |> 
  slice_sample(n=5) |> 
  select(burrows_pond) |> 
  mutate(SMP=TRUE)

mark_selection.join <- mark_selection |> 
  left_join(smp_selection,by="burrows_pond") |> 
  mutate(smp_tag_number=ifelse(SMP==TRUE,300,NA)) |> 
  select(-SMP)





# determine the number needed to tag by release group

# first part is how many total tags are available,
# looks like it's been 32,900

tags_available <- 32900

# not sure why, but the math I'm seeing in the spreadsheet
# is the proportion of the total BY number that are not
# given CWT multiplied by available
# tags

tag_assignment <- sst_ponding.dat |> 
  mutate(by_total=sum(total[cwt==FALSE])) |> 
  group_by(group) |> 
  summarize(group_total=sum(total[cwt==FALSE]),
            by_total=first(by_total),
            group_proportion=group_total/by_total,
            tag_number=tags_available*group_proportion)


# i think normally we'd randomly select the number
# of groups possible from what's available by group

# set number to select here
# 
# direct_ponds <- 9
# 
# redhouse_ponds <- 2
# 
# clear_ponds <- 2

## working through an example where the selection
## already occurred, just to make sure i'm following
# the calculations of that spreadsheet

ponds25 <- c(29,45,47,30,36,38,54,60,68,
             12,16,
             19,2)

selected25 <- sst_ponding.dat |> 
  filter(burrows_pond %in% ponds25)

mark_total25 <- sst_ponding.dat |> 
  mutate(marking=case_when(
    burrows_pond %in% ponds25 ~ TRUE,
    TRUE ~ FALSE
  )) |> 
  group_by(group) |> 
  mutate(release_group_total=sum(total),
         release_group_markponds_total=sum(total[marking==T]),
         proportion_fish=if_else(marking==TRUE,
                                 total/release_group_total,NA),
         proportion_markfish=if_else(marking==TRUE,
                                     total/release_group_markponds_total,NA),
         .groups="drop") |> 
  left_join(tag_assignment,by="group") |> 
  mutate(tagged=proportion_markfish*tag_number,
         tagged_floor=floor(tagged/100)*100)

# tagged summaries by group

group_tag_summaries <- mark_total25 |> 
  group_by(group) |> 
  summarize(marked=sum(tagged_floor,na.rm=T))

######################################################
# Idea for simplified selection and number assignment#
######################################################

# first set number to select by release group


sample_size <- tibble(
  group = c("Direct", "Red House", "Clear Creek"),
  n_selected    = c(9L,2L, 2L)
)

mark_selection <- sst_ponding.dat |> 
  inner_join(sample_size,by="group") |> 
  group_by(group) |> 
  mutate(.rand=runif(n())) |> 
  arrange(.rand,.by_group = TRUE) |> 
  filter(row_number()<=first(n_selected)) |> 
  ungroup() |> 
  select(-.rand,-n_selected)




mark_selection.join <- mark_selection |> 
  left_join(smp_selection,by="burrows_pond")

# now set number of tags by selected pond

pond_numbers <- tibble(group=c("Direct","Red House","Clear Creek"),
                       tags_available=c(18500,6000,6900),
                       selected_ponds=c(9,2,2)) |> 
  mutate(pond_allocation=tags_available/selected_ponds,
         pond_floor=floor(pond_allocation/100)*100)

### probably want divisions in 100s because of how tags come
# pre-loaded


mark_plan <- mark_selection.join |> 
  left_join(pond_numbers,by="group") |> 
  mutate(SMP_tags=ifelse(SMP==TRUE,300,NA))


MINUTES := 5
TARGET := slides
SRC    := $(TARGET).md
PDF    := $(TARGET).pdf
HTML   := $(TARGET).html
THEME  := ../takibi-theme/takibi.css
IMAGES := ../takibi-theme/takibi-icon-back-v3.png ../takibi-theme/takibi-icon-v3.png
# The figures are COPIES of docs/wont-compile/assets/, not references to them.
# A talk is a record of what was said on one day; a catalog figure is
# maintained and may be redrawn. Referencing the live one would silently
# rewrite this deck years after it was given.
FIGURES :=
MEDIA := assets/kverus.png
ASSETS := $(FIGURES) $(MEDIA)
# assets/demo.mp4 and assets/demo.png are recorded separately and dropped in
# by hand, so they are deliberately NOT prerequisites: the deck has to build
# before the capture exists. The theme gives `section.video-slide video` a
# dark box with an ember border and a fixed height, so a missing capture
# renders as an empty frame of the right size rather than as a broken slide.
# demo.png is the poster, and it is the only thing a reader of the exported
# PDF will see, because a PDF cannot play video -- so check that slide by eye
# before exporting.
MARP   := marp $(SRC) --theme-set $(THEME) --allow-local-files --html

# `review` renders every slide to PNG. This is the only way a slide that
# overflows its box gets found: the source looks fine, and Marp does not
# complain -- the content simply runs under the footer or off the bottom.
# Three slides in this deck did that, and all three were caught here.
#
# Marp needs a real Chromium. The `chromium-browser` package on this image is
# a snap stub that cannot start inside a container ("requires the chromium
# snap to be installed"), so install one that works and point CHROME_PATH at
# it:
#
#     npx puppeteer browsers install chrome
#     make review CHROME_PATH=$HOME/.cache/puppeteer/chrome/linux-*/chrome-linux64/chrome
#
REVIEW_DIR := review
DIST := dist
DIST_ASSETS := $(DIST)/assets

.PHONY: all show html pdf bundle review clean

all: show

show:
	RABBIT_MINUTES=$(MINUTES) $(MARP) --engine ./rabbit-engine.js --preview $(SRC)

html: $(HTML)

$(HTML): $(SRC) $(THEME) $(IMAGES) $(FIGURES)
	$(MARP) -o $@

pdf: $(SRC) $(THEME) $(IMAGES) $(FIGURES)
	$(MARP) --pdf

# Create a portable, self-hosted copy for a web server or blog repository.
# Marp inlines the shared CSS but preserves its repository-relative image
# URLs, so retarget those images to copies inside the bundle. Talk-local
# assets already use assets/... in both the source and bundle layouts.
# Unlike the presentation-time targets, the bundle requires the recorded
# video and its PDF poster because it is the complete published artifact.
bundle: $(SRC) $(THEME) $(IMAGES) $(ASSETS)
	mkdir -p $(DIST_ASSETS)
	$(MARP) -o $(DIST)/index.html
	$(MARP) --pdf -o $(DIST)/slides.pdf
	cp $(ASSETS) $(DIST_ASSETS)/
	cp $(IMAGES) $(DIST_ASSETS)/
	sed -i \
		-e 's#../takibi-theme/takibi-icon-v3.png#assets/takibi-icon-v3.png#g' \
		-e 's#../takibi-theme/takibi-icon-back-v3.png#assets/takibi-icon-back-v3.png#g' \
		$(DIST)/index.html
	@if grep -q '../takibi-theme/takibi-icon' $(DIST)/index.html; then \
		echo "ERROR: bundle still contains repository-relative image URLs"; \
		exit 1; \
	fi

review: $(SRC) $(THEME) $(IMAGES) $(FIGURES)
	mkdir -p $(REVIEW_DIR)
	$(MARP) --images png -o $(REVIEW_DIR)/slide.png

clean:
	rm -f $(PDF) $(HTML)
	rm -rf $(REVIEW_DIR) $(DIST)

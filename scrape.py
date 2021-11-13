import re
import requests

from bs4 import BeautifulSoup

URL = "https://www.epicurious.com/search?content=recipe&sort=newest&page="
MAX = 2507


def scrape_urls():
    urls = set()

    for i in range(1, MAX + 1):
        x = requests.get(URL + str(i))
        urls |= set(re.findall(r'/recipes/food/views/[^"]+', x.text))

        print(f"page {i}")

    with open("outputs/urls", "w") as f:
        for x in urls:
            f.write(x + "\n")


def main():
    scrape_urls()

    with open("outputs/urls") as f:
        urls = f.readlines()

    f = open("data/epi.json", "w")
    err = open("outputs/bad-recs", "w")

    for i, url in enumerate(urls):
        print(f"{i}: {url[:-1]}")
        r = requests.get("https://www.epicurious.com" + url[:-1])
        soup = BeautifulSoup(r.text, "html.parser")

        try:
            js = soup.find("script", {"type": "application/ld+json"}).getText()
        except:
            err.write(url)
            print("No json")
            continue

        f.write(js + "\n")

    err.close()
    f.close()


if __name__ == "__main__":
    main()

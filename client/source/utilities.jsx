const kibi = 1024;
const mebi = kibi * kibi;
const gibi = mebi * kibi;
const tebi = gibi * kibi;
const pebi = tebi * kibi;

function round2(x) {
	return Math.round(x * 100) / 100;
}

function formatBytes(num) {
	if (num >= pebi)
		return round2(num / pebi) + " PiB";
	else if (num >= tebi)
		return round2(num / tebi) + " TiB";
	else if (num >= gibi)
		return round2(num / gibi) + " GiB";
	else if (num >= mebi)
		return round2(num / mebi) + " MiB";
	else if (num >= kibi)
		return round2(num / kibi) + " KiB";
	else
		return num + " B";
}

module.exports = {
	formatBytes
};

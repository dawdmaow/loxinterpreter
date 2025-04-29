import adapter from '@sveltejs/adapter-static';
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';

const config = {
	preprocess: vitePreprocess(),
	paths: {
		base: process.argv.includes('dev') ? '' : process.env.BASE_PATH
	},
	kit: { adapter: adapter() }
};

export default config;
